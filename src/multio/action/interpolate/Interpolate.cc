/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 *
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


#include "multio/action/interpolate/Interpolate.h"

#include <algorithm>
#include <regex>
#include <string>
#include <vector>

#include "eckit/exception/Exceptions.h"
#include "eckit/mpi/Comm.h"
#include "eckit/parser/YAMLParser.h"
#include "eckit/types/Fraction.h"

#include "metkit/mars/MarsLanguage.h"

#include "mir/api/MIRJob.h"
#include "mir/input/RawInput.h"
#include "mir/output/ResizableOutput.h"
#include "mir/param/SimpleParametrisation.h"
#include "mir/repres/gauss/reduced/Reduced.h"

#include "multio/LibMultio.h"
#include "multio/message/Message.h"
#include "multio/util/PrecisionTag.h"
#include "multio/util/Substitution.h"


namespace multio::action::interpolate {

namespace {

// Quick and dirty fix to avoid encoding problems with spherical harmonics
const std::vector<std::string> metadata_black_list{"sphericalHarmonics",
                                                   "complexPacking",
                                                   "pentagonalResolutionParameterJ",
                                                   "pentagonalResolutionParameterK",
                                                   "pentagonalResolutionParameterM",
                                                   "subSetJ",
                                                   "subSetK",
                                                   "subSetM"};

const std::vector<double> full_area{90.0, 0.0, -90.0, 360.0};

template <typename DestType>
void forwardMetadata(const eckit::LocalConfiguration& cfg, DestType& destination, const std::string& key,
                     const eckit::Value& value) {
    if (value.isList()) {
        if (value.head().isDouble()) {
            destination.set(key, cfg.getDoubleVector(key));
        }
        else if (value.head().isNumber()) {
            destination.set(key, cfg.getLongVector(key));
        }
        else if (value.isString()) {
            destination.set(key, cfg.getStringVector(key));
        }
        else {
            NOTIMP;
        }
        return;
    }
    if (value.isBool()) {
        destination.set(key, cfg.getBool(key));
    }
    else if (value.isDouble()) {
        destination.set(key, cfg.getDouble(key));
    }
    else if (value.isNumber()) {
        destination.set(key, cfg.getInt(key));
    }
    else if (value.isString()) {
        destination.set(key, cfg.getString(key).c_str());
    }
    else {
        NOTIMP;
    }
};

template <typename DestType>
void regularLatLongMetadata(DestType& param, std::vector<double> grid, std::vector<double> area) {

    auto N = [](double delta, double range) {
        const auto f = eckit::Fraction(range) / eckit::Fraction(delta);
        return static_cast<long>(f.integralPart());
    };

    const double west_east_increment = grid[0];
    const double south_north_increment = grid[1];
    const double north = area[0];
    const double west = area[1];
    const double south = area[2];
    const double east = area[3];

    param.set("gridded", true);
    param.set("gridType", "regular_ll");

    long Ni = N(west_east_increment, std::fabs(east - west));
    long Nj = N(south_north_increment, std::fabs(south - north)) + 1L /* "endpoint" */;
    param.set("west_east_increment", west_east_increment);
    param.set("south_north_increment", south_north_increment);
    param.set("Ni", Ni);
    param.set("Nj", Nj);

    param.set("north", north);
    param.set("west", west);
    param.set("south", south);
    param.set("east", east);

    return;
};

}  // namespace

void fill_out_metadata(const message::Metadata& in_md, message::Metadata& out_md) {
    for (const auto& kv : in_md) {
        if (std::find(metadata_black_list.cbegin(), metadata_black_list.cend(), kv.first)
            == metadata_black_list.cend()) {
            out_md.set(kv.first, kv.second);
        }
    }
    return;
};

eckit::Value getInputGrid(const eckit::LocalConfiguration& cfg, message::Metadata& md) {
    auto searchAtlasGridKind = md.find("atlas-grid-kind");
    if (searchAtlasGridKind != md.end()) {  // metadata has always precedence
        // TODO: name is bad on purpose (no software support this at the moment)
        return searchAtlasGridKind->second.visit(Overloaded{
            [](auto& v) -> util::IfTypeOf<decltype(v), message::MetadataNestedTypes, eckit::Value> { return {}; },
            [](auto& vec) -> util::IfTypeOf<decltype(vec), message::MetadataVectorTypes, eckit::Value> {
                std::vector<eckit::Value> valList;
                valList.reserve(vec.size());
                for (const auto& v : vec) {
                    valList.emplace_back(v);
                }
                return eckit::Value{std::move(valList)};
            },
            [](auto& v) -> util::IfTypeOf<decltype(v), message::MetadataScalarTypes, eckit::Value> {
                return eckit::Value{v};
            }});
    }
    if (cfg.has("input")) {  // configuration file is second option
        return eckit::Value{cfg.getSubConfiguration("input").get()};
    }

    throw eckit::SeriousBug("action-interpolate :: Unable to identify input grid", Here());
}


void fill_input(const eckit::LocalConfiguration& cfg, mir::param::SimpleParametrisation& param, std::string domain,
                const eckit::Value&& inp) {

    auto regular_ll = [&param](double west_east_increment, double south_north_increment) {
        regularLatLongMetadata(param, std::vector<double>{west_east_increment, south_north_increment}, full_area);
    };

    if (inp.isString()) {
        const std::string input = util::replaceCurly(inp.as<std::string>(), [](std::string_view replace) {
            std::string lookUpKey{replace};
            char* env = ::getenv(lookUpKey.c_str());
            return env ? std::optional<std::string>{env} : std::optional<std::string>{};
        });

        static const std::regex sh("(T|TCO|TL)([1-9][0-9]*)");
        static const std::regex gg("([FNO])([1-9][0-9]*)");
        static const std::regex eORCA("^e?ORCA[0-9]+_[FTUVW]$");
        static const std::regex eORCA_fromMetadata("^e?ORCA[0-9]+$");

#define fp "([+]?([0-9]*[.])?[0-9]+([eE][-+][0-9]+)?)"
        static const std::regex ll(fp "/" fp);
#undef fp

        std::smatch match;
        if (std::regex_match(input, match, sh)) {
            param.set("spectral", true);
            param.set("gridType", "sh");
            param.set("truncation", std::stol(match[2].str()));
            return;
        }

        if (std::regex_match(input, match, gg)) {
            param.set("gridded", true);
            param.set("gridType", match[1].str() == "F" ? "regular_gg" : "reduced_gg");
            param.set("N", std::stol(match[2].str()));
            param.set("north", 90.).set("west", 0.).set("south", -90.).set("east", 360.);

            if (match[1].str() != "F") {
                param.set("pl", mir::repres::gauss::reduced::Reduced::pls(match[0].str()));
            }
            return;
        }

        if (std::regex_match(input, match, ll)) {
            regular_ll(std::stod(match[1].str()), std::stod(match[4].str()));
            return;
        }

        if (std::regex_match(input, match, eORCA)) {
            std::string sane_name(input);
            std::transform(sane_name.begin(), sane_name.end(), sane_name.begin(), ::toupper);
            if (sane_name.front() == 'E') {
                sane_name.front() = 'e';
            }
            param.set("gridded", true);
            param.set("uid", sane_name);
            param.set("gridType", "orca");
            return;
        }

        if (std::regex_match(input, match, eORCA_fromMetadata)) {
            std::string kind = domain.substr(0, 1);
            if (kind != "T" && kind != "F" && kind != "U" && kind != "V" && kind != "W") {
                throw eckit::SeriousBug("action-interpolate :: unrecognized orca grid", Here());
            }
            std::string sane_name(input + "_" + kind);
            std::transform(sane_name.begin(), sane_name.end(), sane_name.begin(), ::toupper);
            if (sane_name.front() == 'E') {
                sane_name.front() = 'e';
            }
            param.set("gridded", true);
            param.set("uid", sane_name);
            param.set("gridType", "orca");
            return;
        }
    }

    if (inp.isList()) {
        if (inp.size() == 2) {
            regular_ll(inp[0], inp[1]);
            return;
        }
    }

    NOTIMP;
}


void fill_job(const eckit::LocalConfiguration& cfg, mir::param::SimpleParametrisation& destination,
              message::Metadata& md) {

    static const struct PostProcKeys : std::vector<std::string> {
        PostProcKeys() {
            const auto yaml = eckit::YAMLParser::decodeFile(metkit::mars::MarsLanguage::languageYamlFile());
            for (const auto& key : yaml["_postproc"].keys().as<eckit::ValueList>()) {
                emplace_back(key.as<std::string>());
            }
        }
        bool contains(const std::string& key) const { return std::find(begin(), end(), key) != end(); }
    } postproc;

    ASSERT(not postproc.contains("input"));
    ASSERT(not postproc.contains("options"));

    auto set = [&destination](const eckit::LocalConfiguration& cfg, const std::string& key, const eckit::Value& value) {
        forwardMetadata<mir::param::SimpleParametrisation>(cfg, destination, key, value);
    };

    for (const auto& key : postproc) {
        if (cfg.has(key)) {
            set(cfg, key, cfg.getSubConfiguration(key).get());
        }
    }


    if (cfg.has("options")) {
        const auto& options = cfg.getSubConfiguration("options");
        for (const auto& key : options.keys()) {
            set(options, key, options.getSubConfiguration(key).get());
        }
    }

    // Handle Output metadata of the interpolated fields
    if (cfg.has("grid")) {
        std::string gridKind("none");
        std::vector<double> grid(2, 0.0);

        if (cfg.getSubConfiguration("grid").get().isString()) {
#define fp "([+]?([0-9]*[.])?[0-9]+([eE][-+][0-9]+)?)"
            static const std::regex ll(fp "/" fp);
            static const std::regex H("([h|H])([1-9][0-9]*)");
#undef fp
            std::smatch matchll;
            std::smatch matchH;
            const auto input = cfg.getString("grid");
            LOG_DEBUG_LIB(LibMultio) << " Grid is a string ::" << input << std::endl;
            if (std::regex_match(input, matchll, ll)) {
                gridKind = "regular_ll";
                grid[0] = std::stod(matchll[1].str());
                grid[1] = std::stod(matchll[4].str());
            }
            if (std::regex_match(input, matchH, H)) {
                gridKind = "HEALPix";
                grid[0] = std::stod(matchH[2].str());
            }
        }
        else if (cfg.getSubConfiguration("grid").get().isList()
                 && cfg.getSubConfiguration("grid").get().head().isDouble()) {
            gridKind = "regular_ll";
            grid = cfg.getDoubleVector("grid");
            LOG_DEBUG_LIB(LibMultio) << " Grid is a list (" << gridKind << ")" << grid << std::endl;
        }
        //
        LOG_DEBUG_LIB(LibMultio) << " Grid Kind is ::" << gridKind << std::endl;
        // Set the mir keywords
        if (gridKind == "regular_ll") {
            LOG_DEBUG_LIB(LibMultio) << " + Set metadata for regular_ll grid" << std::endl;
            if (cfg.has("area")) {
                const auto& area = cfg.getDoubleVector("area");
                regularLatLongMetadata<message::Metadata>(md, grid, area);
            }
            else {
                regularLatLongMetadata<message::Metadata>(md, grid, full_area);
            }
        }
        else if (gridKind == "HEALPix") {
            //
            md.set("gridded", true);
            md.set("gridType", "HEALPix");
            md.set("Nside", grid[0]);
            md.set("orderingConvention", "ring");
        }
        else {
            std::cout << "Grid not implemented" << std::endl;
        }
    }
}

template <>
message::Message Interpolate::InterpolateMessage<double>(message::Message&& msg) const {
    LOG_DEBUG_LIB(LibMultio) << "Interpolate :: Metadata of the input message :: " << std::endl
                             << msg.metadata() << std::endl
                             << std::endl;

    const auto& config = Action::compConf_.parsedConfig();

    const double* data = reinterpret_cast<const double*>(msg.payload().data());
    const size_t size = msg.payload().size() / sizeof(double);

    message::Metadata md;
    fill_out_metadata(msg.metadata(), md);
    md.set("precision", "double");

    mir::param::SimpleParametrisation inputPar;
    fill_input(config, inputPar, msg.domain(), getInputGrid(config, md));
    auto searchMissingValue = msg.metadata().find("missingValue");
    auto searchBitmapPresent = msg.metadata().find("bitmapPresent");
    if (searchMissingValue != msg.metadata().end() && searchBitmapPresent != msg.metadata().end()) {
        inputPar.set("missing_value", searchMissingValue->second.get<double>());
    }

    mir::input::RawInput input(data, size, inputPar);

    mir::api::MIRJob job;
    fill_job(config, job, md);

    LOG_DEBUG_LIB(LibMultio) << "Interpolate :: input :: " << std::endl << inputPar << std::endl << std::endl;

    LOG_DEBUG_LIB(LibMultio) << "Interpolate :: job " << std::endl << job << std::endl << std::endl;

    std::vector<double> outData;
    mir::param::SimpleParametrisation outMetadata;
    mir::output::ResizableOutput output(outData, outMetadata);

    // TODO: Probably this operation needs to be done when plans are called, in this way
    //       it is valid for all the IO actions as it should be
    auto& originalComm = eckit::mpi::comm();
    eckit::mpi::setCommDefault("self");
    job.execute(input, output);
    eckit::mpi::setCommDefault(originalComm.name().c_str());
    md.set<long>("globalSize", outData.size());

    // Forward the metadata from mir to multIO (at the moment only missingValue)
    if (outMetadata.has("missing_value")) {
        double v;
        outMetadata.get("missing_value", v);
        md.set("missingValue", v);
        md.set("bitmapPresent", 1);
    }

    eckit::Buffer buffer(reinterpret_cast<const char*>(outData.data()), outData.size() * sizeof(double));

    LOG_DEBUG_LIB(LibMultio) << "Interpolate :: Metadata of the output message :: " << std::endl
                             << md << std::endl
                             << std::endl;


    return {message::Message::Header{message::Message::Tag::Field, msg.source(), msg.destination(), std::move(md)},
            std::move(buffer)};
}

template <>
message::Message Interpolate::InterpolateMessage<float>(message::Message&& msg) const {
    // convert single/double precision, interpolate, convert double/single
    return InterpolateMessage<double>(message::convert_precision<float, double>(std::move(msg)));
}

void Interpolate::executeImpl(message::Message msg) {

    if (msg.tag() != message::Message::Tag::Field) {
        executeNext(msg);
        return;
    }

    executeNext(util::dispatchPrecisionTag(msg.precision(), [&](auto pt) -> message::Message {
        using Precision = typename decltype(pt)::type;
        return InterpolateMessage<Precision>(std::move(msg));
    }));
}


void Interpolate::print(std::ostream& os) const {
    os << "Interpolate";
}


static ActionBuilder<Interpolate> InterpolateBuilder("interpolate");


}  // namespace multio::action::interpolate
