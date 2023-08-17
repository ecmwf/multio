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

    param.set("north", north).set("west", west).set("south", south).set("east", east);

    return;
};

}  // namespace

void fill_out_metadata(const message::Metadata& in_md, message::Metadata& out_md) {
    for (auto& key : in_md.keys()) {
        if (std::find(metadata_black_list.cbegin(), metadata_black_list.cend(), key) == metadata_black_list.cend()) {
            forwardMetadata<message::Metadata>(in_md, out_md, key, in_md.getSubConfiguration(key).get());
        }
    }
    return;
};

eckit::Value getInputGrid(const eckit::LocalConfiguration& cfg, message::Metadata& md) {
    if (md.has("atlas-grid-kind")) {  // metadata has always precedence
        // TODO: name is bad on purpose (no software support this at the moment)
        return eckit::Value{md.getSubConfiguration("atlas-grid-kind").get()};
    }

    if (cfg.has("input")) {  // configuration file is second option
        return eckit::Value{cfg.getSubConfiguration("input").get()};
    }

    throw eckit::SeriousBug("action-interpolate :: Unable to identify input grid", Here());
}


void fill_input(const eckit::LocalConfiguration& cfg, mir::param::SimpleParametrisation& param,
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

    if (cfg.has("grid")) {
        std::vector<double> grid(2, 0.0);
        if (cfg.getSubConfiguration("grid").get().isString()) {
#define fp "([+]?([0-9]*[.])?[0-9]+([eE][-+][0-9]+)?)"
            static const std::regex ll(fp "/" fp);
#undef fp
            std::smatch match;
            const auto input = cfg.getString("grid");
            if (std::regex_match(input, match, ll)) {
                grid[0] = std::stod(match[1].str());
                grid[1] = std::stod(match[4].str());
            }
        }
        else if (cfg.getSubConfiguration("grid").get().isList()
                 && cfg.getSubConfiguration("grid").get().head().isDouble()) {
            grid = cfg.getDoubleVector("grid");
        }
        if (cfg.has("area")) {
            const auto& area = cfg.getDoubleVector("area");
            regularLatLongMetadata<message::Metadata>(md, grid, area);
        }
        else {
            regularLatLongMetadata<message::Metadata>(md, grid, full_area);
        }
        // Allow the encoder to work on regridded grids
        md.set("gridType", "regular_ll");
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
    fill_input(config, inputPar, getInputGrid(config, md));
    if (msg.metadata().has("missingValue") && msg.metadata().getBool("bitmapPresent")) {
        inputPar.set("missing_value", msg.metadata().getDouble("missingValue"));
    }

    mir::input::RawInput input(data, size, inputPar);

    mir::api::MIRJob job;
    fill_job(config, job, md);


    LOG_DEBUG_LIB(LibMultio) << "Interpolate :: input :: " << std::endl << inputPar << std::endl << std::endl;

    LOG_DEBUG_LIB(LibMultio) << "Interpolate :: job " << std::endl << job << std::endl << std::endl;

    std::vector<double> outData;
    mir::param::SimpleParametrisation outMetadata;
    mir::output::ResizableOutput output(outData, outMetadata);

    // TODO: Probably this operation needs to be when to plans are called, in this way
    //       it is walid for all the IO actions as it should be
    auto& originalComm = eckit::mpi::comm();
    eckit::mpi::setCommDefault("self");
    job.execute(input, output);
    eckit::mpi::setCommDefault(originalComm.name().c_str());
    md.set("globalSize", outData.size());


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
