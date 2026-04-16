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
#include <array>
#include <iomanip>
#include <regex>
#include <sstream>
#include <string>
#include <vector>

#include "Interpolate.h"
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
#include "multio/datamod/Glossary.h"
#include "multio/message/Message.h"
#include "multio/util/PrecisionTag.h"
#include "multio/util/Substitution.h"


namespace multio::action::interpolate {

namespace dm = multio::datamod;

using message::MetadataTypes;

namespace {

// Quick and dirty fix to avoid encoding problems with spherical harmonics
const std::vector<typename MetadataTypes::KeyType> metadata_black_list{dm::legacy::SphericalHarmonics,
                                                                       dm::legacy::ComplexPacking,
                                                                       dm::legacy::PentagonalResolutionParameterJ,
                                                                       dm::legacy::PentagonalResolutionParameterK,
                                                                       dm::legacy::PentagonalResolutionParameterM,
                                                                       dm::legacy::SubSetJ,
                                                                       dm::legacy::SubSetK,
                                                                       dm::legacy::SubSetM};

const std::array<double, 4> full_area{90.0, 0.0, -90.0, 360.0};

void forwardMetadata(const eckit::LocalConfiguration& cfg, mir::param::SimpleParametrisation& destination,
                     const std::string& key) {
    if (cfg.isFloatingPointList(key)) {
        destination.set(key, cfg.getDoubleVector(key));
        return;
    }
    if (cfg.isIntegralList(key)) {
        destination.set(key, cfg.getLongVector(key));
        return;
    }
    if (cfg.isStringList(key)) {
        destination.set(key, cfg.getStringVector(key));
        return;
    }
    if (cfg.isBoolean(key)) {
        destination.set(key, cfg.getBool(key));
        return;
    }
    if (cfg.isFloatingPoint(key)) {
        destination.set(key, cfg.getDouble(key));
        return;
    }
    if (cfg.isIntegral(key)) {
        destination.set(key, cfg.getLong(key));
        return;
    }
    if (cfg.isString(key)) {
        destination.set(key, cfg.getString(key));
        return;
    }
    NOTIMP;
};

void setMirMetadata(mir::param::SimpleParametrisation& destination, const std::string& key,
                    const std::array<double, 4>& value) {
    destination.set(key, std::vector<double>{value.begin(), value.end()});
}

void setMirMetadata(mir::param::SimpleParametrisation& destination, const std::string& key, const std::string& value) {
    destination.set(key, value);
}

void setMirMetadata(mir::param::SimpleParametrisation& destination, const std::string& key, std::int64_t value) {
    destination.set(key, value);
}

void setMirMetadata(mir::param::SimpleParametrisation& destination, const std::string& key,
                    const eckit::LocalConfiguration& value) {
    for (const auto& subKey : value.keys()) {
        forwardMetadata(value, destination, subKey);
    }
}

void setMirMetadata(mir::param::SimpleParametrisation& destination, const std::string& key,
                    const AdditionalMetadata& value) {
    // Ignore additional metadata for MIR
}

template <typename T>
void setMirMetadata(mir::param::SimpleParametrisation& destination, const std::string& key,
                    const std::optional<T>& value) {
    if (value) {
        setMirMetadata(destination, key, *value);
    }
}


template <typename DestType>
void regularLatLongMetadata(DestType& param, std::array<double, 2> grid, std::array<double, 4> area) {

    auto N = [](double delta, double range) {
        const auto f = eckit::Fraction(range) / eckit::Fraction(delta);
        return static_cast<std::int64_t>(f.integralPart());
    };

    const double west_east_increment = grid[0];
    const double south_north_increment = grid[1];
    const double north = area[0];
    const double west = area[1];
    const double south = area[2];
    const double east = area[3];

    param.set("gridded", true);
    param.set("gridType", "regular_ll");

    std::int64_t Ni = N(west_east_increment, std::fabs(east - west));
    std::int64_t Nj = N(south_north_increment, std::fabs(south - north)) + 1L /* "endpoint" */;
    param.set("west_east_increment", west_east_increment);
    param.set("south_north_increment", south_north_increment);
    param.set("Ni", Ni);
    param.set("Nj", Nj);

    param.set("north", north);
    param.set("west", west);
    param.set("south", south);
    param.set("east", east);
};


std::string fesomCacheName(const std::string& fesomName, const std::string& domain, const std::string& precision,
                           size_t NSide, const std::string& orderingConvention, double level) {
    std::ostringstream os;
    std::string localDomain{domain};
    localDomain.erase(std::remove_if(localDomain.begin(), localDomain.end(), ::isspace), localDomain.end());
    std::transform(localDomain.begin(), localDomain.end(), localDomain.begin(),
                   [](unsigned char c) { return std::tolower(c); });
    os << "fesom_" << fesomName << "_" << localDomain << "_to_HEALPix_" << std::setw(6) << std::setfill('0') << NSide
       << "_" << precision << "_" << orderingConvention << "_" << std::setw(8) << std::setfill('0')
       << static_cast<size_t>(std::fabs(level * 1000)) << ".mat";
    return os.str();
}

template <typename T>
std::string generateKey(const message::Message& msg, const std::string& cache_path, size_t NSide,
                        const std::string& orderingConvention) {
    // TODO: Probably missing the kind of fesom grid in the name
    //       need to see the metadata to understand how to extract it
    size_t level = static_cast<size_t>(
        msg.metadata().getOpt<std::int64_t>("level").value_or(msg.metadata().getOpt<double>("levelist").value_or(0)));
    if ((msg.metadata().get<std::string>("category") == "ocean-3d")
        && (msg.metadata().get<std::string>("fesomLevelType") == "level")) {
        if (level == 0) {
            std::ostringstream os;
            os << " - Wrong level for the oceal level" << std::endl;
            throw eckit::SeriousBug(os.str(), Here());
        }
        level--;
    }
    auto searchUnstructuredGridType = msg.metadata().find("unstructuredGridType");
    if (searchUnstructuredGridType == msg.metadata().end()) {
        std::ostringstream os;
        os << " - \"unstructuredGridType\" not present in the metadata" << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    std::string fesomGridName = searchUnstructuredGridType->second.get<std::string>();
    std::string key = fesomCacheName(fesomGridName, msg.domain(), (sizeof(T) == 4 ? "single" : "double"), NSide,
                                     orderingConvention, level);

    return cache_path + "/" + key;
}

}  // namespace


Interpolate::Interpolate(const ComponentConfiguration& compConf) :
    ChainedAction(compConf), opts_{cf::parseActionConfig<Options>(compConf)} {}


void fill_out_metadata(const message::Metadata& in_md, message::Metadata& out_md) {
    for (const auto& kv : in_md) {
        if (std::find(metadata_black_list.cbegin(), metadata_black_list.cend(), kv.first)
            == metadata_black_list.cend()) {
            out_md.set(kv.first, kv.second);
        }
    }
};

void fill_input(const Options& opts, mir::param::SimpleParametrisation& param, size_t payload_size,
                std::string domain) {

    auto regular_ll = [&param](double west_east_increment, double south_north_increment) {
        regularLatLongMetadata(param, {west_east_increment, south_north_increment}, full_area);
    };

    static const std::regex sh("(T|TCO|TL)([1-9][0-9]*)");
    static const std::regex gg("([FNO])([1-9][0-9]*)");
    static const std::regex eORCA("^e?ORCA[0-9]+_[FTUVW]$");
    static const std::regex eORCA_fromMetadata("^e?ORCA[0-9]+$");
    static const std::regex FESOM("^fesom$");

#define fp "([+]?([0-9]*[.])?[0-9]+([eE][-+][0-9]+)?)"
    static const std::regex ll(fp "/" fp);
#undef fp

    std::smatch match;
    if (std::regex_match(opts.input, match, sh)) {
        param.set("spectral", true);
        param.set("gridType", "sh");
        param.set("truncation", std::stol(match[2].str()));
        return;
    }

    if (std::regex_match(opts.input, match, gg)) {
        param.set("gridded", true);
        param.set("gridType", match[1].str() == "F" ? "regular_gg" : "reduced_gg");
        param.set("N", std::stol(match[2].str()));
        param.set("north", 90.).set("west", 0.).set("south", -90.).set("east", 360.);

        if (match[1].str() != "F") {
            param.set("pl", mir::repres::gauss::reduced::Reduced::pls(match[0].str()));
        }
        return;
    }

    if (std::regex_match(opts.input, match, ll)) {
        LOG_DEBUG_LIB(LibMultio) << "matched ll:" << std::endl;
        regular_ll(std::stod(match[1].str()), std::stod(match[4].str()));
        return;
    }

    if (std::regex_match(opts.input, match, eORCA)) {
        std::string sane_name(opts.input);
        std::transform(sane_name.begin(), sane_name.end(), sane_name.begin(), ::toupper);
        if (sane_name.front() == 'E') {
            sane_name.front() = 'e';
        }
        param.set("gridded", true);
        param.set("uid", sane_name);
        param.set("gridType", "orca");
        return;
    }

    if (std::regex_match(opts.input, match, FESOM)) {
        param.set("gridded", true);
        param.set("gridType", "unstructured_grid");
        param.set("numberOfPoints", payload_size);
        return;
    }

    if (std::regex_match(opts.input, match, eORCA_fromMetadata)) {
        if (domain.empty()) {
            throw eckit::SeriousBug(
                "action-interpolate :: domain is empty - can not infere which orca grid is being used", Here());
        }

        std::string kind = domain.substr(0, 1);
        if (kind != "T" && kind != "F" && kind != "U" && kind != "V" && kind != "W") {
            throw eckit::SeriousBug("action-interpolate :: unrecognized orca grid", Here());
        }
        std::string sane_name(opts.input + "_" + kind);
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

void fill_job(const Options& opts, mir::param::SimpleParametrisation& destination, message::Metadata& md,
              const message::Message& msg) {

    static const struct PostProcKeys : std::vector<std::string> {
        PostProcKeys() {
            const auto yaml = eckit::YAMLParser::decodeFile(metkit::mars::MarsLanguage::languageYamlFile());
            std::vector<std::string> missingKeys;

            for (const auto& key : yaml["_postproc"].keys().as<eckit::ValueList>()) {
                std::string keyStr = key.as<std::string>();
                emplace_back(keyStr);
                if (!cf::detail::containsKey<Options>(keyStr)) {
                    missingKeys.push_back(keyStr);
                }
            }

            if (!missingKeys.empty()) {
                std::ostringstream oss;
                oss << "Interpolate option is not supporting all postproc keys - " << missingKeys.size()
                    << " key are missing: " << missingKeys;
                throw eckit::SeriousBug(oss.str(), Here());
            };
        }
        bool contains(const std::string& key) const { return std::find(begin(), end(), key) != end(); }
    } postproc;

    ASSERT(not postproc.contains("input"));
    ASSERT(not postproc.contains("options"));


    util::forEach(
        [&](const auto& field) {
            std::string key(field.key);
            if (postproc.contains(key)) {
                setMirMetadata(destination, key, field.get(opts));
            }
        },
        Options::fields_);

    if (opts.options) {
        for (const auto& subKey : opts.options->keys()) {
            forwardMetadata(*opts.options, destination, subKey);
        }
    }

    // Forward to mir the options
    bool interpolationMatrix
        = (opts.options && opts.options->has("interpolation") && opts.options->getString("interpolation") == "matrix");

    // Handle Output metadata of the interpolated fields
    if (opts.grid) {
#define fp "([+]?([0-9]*[.])?[0-9]+([eE][-+][0-9]+)?)"
        static const std::regex ll(fp "/" fp);
        static const std::regex H("([h|H])([1-9][0-9]*)(_nested)?");
#undef fp
        std::smatch matchll;
        std::smatch matchH;

        LOG_DEBUG_LIB(LibMultio) << " Grid is a string ::" << *opts.grid << std::endl;
        if (std::regex_match(*opts.grid, matchll, ll)) {
            std::string gridKind = "regular_ll";
            LOG_DEBUG_LIB(LibMultio) << " Grid Kind is ::" << gridKind << std::endl;
            std::array<double, 2> grid{std::stod(matchll[1].str()), std::stod(matchll[4].str())};

            LOG_DEBUG_LIB(LibMultio) << " + Set metadata for regular_ll grid" << std::endl;
            regularLatLongMetadata<message::Metadata>(md, grid, opts.area ? *opts.area : full_area);

            if (interpolationMatrix) {
                std::ostringstream os;
                os << " - interpolation matrix supported only for fesom -> Healpix" << std::endl;
                throw eckit::SeriousBug(os.str(), Here());
            }
        }
        else if (std::regex_match(*opts.grid, matchH, H)) {
            std::string gridKind = matchH[3].str() == "_nested" ? "HEALPix_nested" : "HEALPix";
            LOG_DEBUG_LIB(LibMultio) << " Grid Kind is ::" << gridKind << std::endl;

            std::int64_t nside = std::stol(matchH[2].str());

            md.set(dm::legacy::Gridded, true);
            md.set(dm::legacy::GridType, "HEALPix");
            md.set(dm::legacy::Nside, nside);
            md.set(dm::legacy::OrderingConvention, gridKind == "HEALPix_nested" ? "nested" : "ring");

            // If no interpolation matrix name is provided, generate one
            if (interpolationMatrix) {
                if (opts.input != "fesom") {
                    std::ostringstream os;
                    os << " - interpolation matrix supported only for fesom -> Healpix" << std::endl;
                    throw eckit::SeriousBug(os.str(), Here());
                }
                const auto weights_file = generateKey<double>(msg, opts.cachePath.value_or(""), nside,
                                                              gridKind == "HEALPix_nested" ? "nested" : "ring");
                destination.set("interpolation-matrix", weights_file);
            }
        }
        else {
            // Set the mir keywords
            throw eckit::UserError("action-interpolate :: Grid not implemented", Here());
        }
    }
}

template <>
message::Message Interpolate::interpolateMessage<double>(message::Message&& msg) const {
    LOG_DEBUG_LIB(LibMultio) << "Interpolate :: Metadata of the input message :: " << std::endl
                             << msg.metadata() << std::endl
                             << std::endl;

    const double* data = reinterpret_cast<const double*>(msg.payload().data());
    const size_t size = msg.payload().size() / sizeof(double);

    message::Metadata md;
    fill_out_metadata(msg.metadata(), md);
    md.set(dm::legacy::Precision, "double");

    if (opts_.grid) {
        md.set("grid", *opts_.grid);
    }

    if (opts_.additionalMetadata) {
        if (opts_.additionalMetadata->resolution) {
            for (const auto& pair : cf::detail::EnumTrait<ResolutionKeyword>::values) {
                if (pair.first == opts_.additionalMetadata->resolution) {
                    md.set("resolution", pair.second);
                    break;
                }
            }
        }
    }

    mir::param::SimpleParametrisation inputPar;
    fill_input(opts_, inputPar, size, msg.metadata().getOpt<std::string>(dm::legacy::Domain).value_or(""));
    auto searchMissingValue = msg.metadata().find("missingValue");
    auto searchBitmapPresent = msg.metadata().find("bitmapPresent");
    if (searchMissingValue != msg.metadata().end() && searchBitmapPresent != msg.metadata().end()
        && searchBitmapPresent->second.get<bool>()) {
        inputPar.set("missing_value", searchMissingValue->second.get<double>());
    }
    else if (opts_.options && opts_.options->has("missing_value")) {
        inputPar.set("missing_value", opts_.options->getDouble("missing_value"));
    }

    mir::input::RawInput input(data, size, inputPar);

    mir::api::MIRJob job;
    fill_job(opts_, job, md, msg);

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
    md.set<std::int64_t>("misc-globalSize", outData.size());

    // Forward the metadata from mir to multIO (at the moment only missingValue)
    if (outMetadata.has("missing_value")) {
        double v;
        outMetadata.get("missing_value", v);
        md.set(dm::legacy::MissingValue, v);
        md.set(dm::legacy::BitmapPresent, true);
    }

    eckit::Buffer buffer(reinterpret_cast<const char*>(outData.data()), outData.size() * sizeof(double));

    LOG_DEBUG_LIB(LibMultio) << "Interpolate :: Metadata of the output message :: " << std::endl
                             << md << std::endl
                             << std::endl;


    return {message::Message::Header{message::Message::Tag::Field, msg.source(), msg.destination(), std::move(md)},
            std::move(buffer)};
}

template <>
message::Message Interpolate::interpolateMessage<float>(message::Message&& msg) const {
    // convert single/double precision, interpolate, convert double/single
    return interpolateMessage<double>(message::convert_precision<float, double>(std::move(msg)));
}

void Interpolate::executeImpl(message::Message msg) {

    if (msg.tag() != message::Message::Tag::Field) {
        executeNext(msg);
        return;
    }

    executeNext(util::dispatchPrecisionTag(msg.precision(), [&](auto pt) -> message::Message {
        using Precision = typename decltype(pt)::type;
        return interpolateMessage<Precision>(std::move(msg));
    }));
}


void Interpolate::print(std::ostream& os) const {
    os << "Interpolate";
}


static ActionBuilder<Interpolate> InterpolateBuilder("interpolate");


}  // namespace multio::action::interpolate
