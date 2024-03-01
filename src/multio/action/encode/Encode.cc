/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Encode.h"

#include <iostream>

#include <regex>

#include "eckit/exception/Exceptions.h"
#include "eckit/io/StdFile.h"
#include "eckit/log/Log.h"
#include "eckit/mpi/Comm.h"
#include "eckit/value/Value.h"


#include "atlas/grid.h"
#include "atlas/library.h"
#include "atlas/parallel/mpi/mpi.h"

#include "GridDownloader.h"
#include "multio/LibMultio.h"
#include "multio/config/ConfigurationPath.h"
#include "multio/util/ScopedTimer.h"

namespace multio::action {

using config::configuration_path_name;

namespace {

void handleCodesError(const std::string& errorPrefix, int error, const eckit::CodeLocation& codeLocation) {
    if (error) {
        std::ostringstream oss;
        oss << errorPrefix << codes_get_error_message(error);
        throw eckit::Exception(oss.str(), codeLocation);
    }
}

atlas::Grid readGrid(const std::string& name) {
    atlas::mpi::Scope mpi_scope("self");
    return atlas::Grid{name};
}

template <class GridType>
GridType createGrid(const std::string& atlasNamedGrid) {

    const atlas::Grid grid = readGrid(atlasNamedGrid);

    auto structuredGrid = atlas::StructuredGrid(grid);

    return GridType(structuredGrid);
}

void updateGaussianGrid(codes_handle* handle, const std::string& atlasNamedGrid) {
    const auto gaussianGrid = createGrid<atlas::GaussianGrid>(atlasNamedGrid);

    int err = codes_set_long(handle, "N", gaussianGrid.N());
    handleCodesError("eccodes error while setting the N value: ", err, Here());

    auto tmp = gaussianGrid.nx();
    std::vector<long> pl(tmp.size(), 0);
    for (int i = 0; i < tmp.size(); ++i) {
        pl[i] = long(tmp[i]);
    }

    err = codes_set_long_array(handle, "pl", pl.data(), pl.size());
    handleCodesError("eccodes error while setting the PL array: ", err, Here());

    std::vector<double> values(gaussianGrid.size(), 0.0);

    auto it = gaussianGrid.lonlat().begin();
    err = codes_set_double(handle, "latitudeOfFirstGridPointInDegrees", (*it)[1]);
    handleCodesError("eccodes error while setting the latitudeOfFirstGridPointInDegrees: ", err, Here());
    err = codes_set_double(handle, "longitudeOfFirstGridPointInDegrees", (*it)[0]);
    handleCodesError("eccodes error while setting the longitudeOfFirstGridPointInDegrees: ", err, Here());
    it += gaussianGrid.size() - 1;
    err = codes_set_double(handle, "latitudeOfLastGridPointInDegrees", (*it)[1]);
    handleCodesError("eccodes error while setting the latitudeOfLastGridPointInDegrees: ", err, Here());

    err = codes_set_double_array(handle, "values", values.data(), values.size());
    handleCodesError("eccodes error while setting the values array: ", err, Here());

    const auto equator = gaussianGrid.N();
    const auto maxLongitude = gaussianGrid.x(gaussianGrid.nx(equator) - 1, equator);

    err = codes_set_double(handle, "longitudeOfLastGridPointInDegrees", maxLongitude);
    handleCodesError("eccodes error while setting the longitudeOfLastGridPointInDegrees value: ", err, Here());
}

void updateRegularLatLonGrid(codes_handle* handle, const std::string& atlasNamedGrid) {
    const auto llGrid = createGrid<atlas::RegularLonLatGrid>(atlasNamedGrid);
    int err = codes_set_long(handle, "Ni", llGrid.nx());
    handleCodesError("eccodes error while setting the Ni value: ", err, Here());
    err = codes_set_long(handle, "Nj", llGrid.ny());
    handleCodesError("eccodes error while setting the Nj value: ", err, Here());
}

using UpdateFunctionType = std::function<void(codes_handle*, const std::string&)>;
static const std::unordered_map<std::string, UpdateFunctionType> updateFunctionMap{
    {"^\\s*[FON]\\d+\\s*$", &updateGaussianGrid}, {"^\\s*L\\d+x\\d+\\s*$", &updateRegularLatLonGrid}};

eckit::LocalConfiguration getEncodingConfiguration(const ComponentConfiguration& compConf) {
    if (compConf.parsedConfig().has("encoding")) {
        return compConf.parsedConfig().getSubConfiguration("encoding");
    }
    else {
        return compConf.parsedConfig();
    }
}

std::unique_ptr<GribEncoder> makeEncoder(const eckit::LocalConfiguration& conf,
                                         const config::MultioConfiguration& multioConfig) {
    auto format = conf.getString("format");

    if (format == "grib") {
        ASSERT(conf.has("template"));
        std::string tmplPath = conf.getString("template");
        // TODO provide utility to distinguish between relative and absolute paths
        eckit::AutoStdFile fin{multioConfig.replaceCurly(tmplPath)};
        int err;
        auto sample = codes_handle_new_from_file(nullptr, fin, PRODUCT_GRIB, &err);
        handleCodesError("eccodes error while reading the grib template: ", err, Here());

        if (conf.has("atlas-named-grid")) {
            const auto atlasNamedGrid = conf.getString("atlas-named-grid");

            eckit::Log::info() << "REQUESTED ATLAS GRID DEFINITION UPDATE: " << atlasNamedGrid << std::endl;

            const auto updateFunction = std::find_if(updateFunctionMap.cbegin(), updateFunctionMap.cend(),
                                                     [&atlasNamedGrid](const auto& item) {
                                                         std::regex r{item.first};
                                                         return std::regex_match(atlasNamedGrid, r);
                                                     });

            if (updateFunction != updateFunctionMap.cend()) {
                updateFunction->second(sample, atlasNamedGrid);
            }
        }

        return std::make_unique<GribEncoder>(sample, conf);
    }
    else if (format == "raw") {
        return nullptr;  // leave message in raw binary format
    }
    else {
        throw eckit::SeriousBug("Encoding format <" + format + "> is not supported");
    }
}

std::string encodingExceptionReason(const std::string& r) {
    std::string s("Enocding exception: ");
    s.append(r);
    return s;
}
}  // namespace


EncodingException::EncodingException(const std::string& r, const eckit::CodeLocation& l) :
    eckit::Exception(encodingExceptionReason(r), l) {}

using message::Message;
using message::Peer;

CodesOverwrites makeOverwrites(const eckit::LocalConfiguration& encConf) {
    CodesOverwrites res{};
    if (encConf.get().isMap()) {
        for (const std::string& k : encConf.keys()) {
            auto val = encConf.getSubConfiguration(k).get();

            if (val.isBool()) {
                res.emplace_back(k, (std::int64_t)val);
            }
            else if (val.isNumber()) {
                res.emplace_back(k, (std::int64_t)val);
            }
            else if (val.isDouble()) {
                res.emplace_back(k, (double)val);
            }
            else if (val.isString()) {
                res.emplace_back(k, (std::string)val);
            }
            else {
                NOTIMP;
            }
        }
    }
    else if (encConf.get().isList()) {
        for (const auto& subConf : encConf.getSubConfigurations()) {
            for (const std::string& k : subConf.keys()) {
                auto val = subConf.getSubConfiguration(k).get();

                if (val.isBool()) {
                    res.emplace_back(k, (std::int64_t)val);
                }
                else if (val.isNumber()) {
                    res.emplace_back(k, (std::int64_t)val);
                }
                else if (val.isDouble()) {
                    res.emplace_back(k, (double)val);
                }
                else if (val.isString()) {
                    res.emplace_back(k, (std::string)val);
                }
                else {
                    NOTIMP;
                }
            }
        }
    }
    return res;
}

Encode::Encode(const ComponentConfiguration& compConf, const eckit::LocalConfiguration& encConf) :
    ChainedAction{compConf},
    format_{encConf.getString("format")},
    overwrite_{makeOverwrites(encConf.has("overwrite")
                                  ? eckit::LocalConfiguration{encConf.getSubConfiguration("overwrite")}
                                  : eckit::LocalConfiguration{})},
    additionalMetadata_{encConf.has("additional-metadata")
                            ? eckit::LocalConfiguration{encConf.getSubConfiguration("additional-metadata")}
                            : (encConf.has("run") ? eckit::LocalConfiguration{encConf.getSubConfiguration("run")}
                                                  : eckit::LocalConfiguration{})},
    encoder_{makeEncoder(encConf, compConf.multioConfig())},
    gridDownloader_{std::make_unique<multio::action::GridDownloader>(compConf)} {}

Encode::Encode(const ComponentConfiguration& compConf) : Encode(compConf, getEncodingConfiguration(compConf)) {}

void Encode::executeImpl(Message msg) {
    if (msg.tag() != Message::Tag::Field) {
        executeNext(std::move(msg));
        return;
    }
    if (not encoder_) {
        executeNext(std::move(msg));
        return;
    }

    auto gridUID = std::optional<GridDownloader::GridUIDType>{};

    if (msg.metadata().has("domain") && !msg.metadata().has("uuidOfHGrid") && isOcean(msg.metadata())) {
        //! TODO shoud not be checked here anymore, encoder_ should have been initialized according to format_
        ASSERT(format_ == "grib");

        LOG_DEBUG_LIB(LibMultio) << " *** Looking for grid info for subtype: " << msg.domain() << std::endl;

        const auto& md = msg.metadata();

        std::string gridType;
        const auto hasGridType = md.get("gridType", gridType);
        if (hasGridType && (gridType == "unstructured_grid")) {
            auto gridCoords
                = gridDownloader_->getGridCoords(msg.domain(), md.getInt32("startDate"), md.getInt32("startTime"));
            if (gridCoords) {
                executeNext(gridCoords.value().Lat);
                executeNext(gridCoords.value().Lon);
            }
        }

        gridUID = gridDownloader_->getGridUID(msg.domain());
    }

    executeNext(encodeField(std::move(msg), gridUID));
}

void Encode::print(std::ostream& os) const {
    os << "Encode(format=" << format_ << ", "
       << "encoder=";
    if (encoder_)
        encoder_->print(os);
    os << ")";
}


message::Message Encode::encodeField(const message::Message& msg, const std::optional<std::string>& gridUID) const {
    try {
        util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};
        auto md = msg.metadata();
        if (gridUID) {
            md.set("uuidOfHGrid", gridUID.value());
        }
        return encoder_->encodeField(msg.modifyMetadata(std::move(md)), this->overwrite_, this->additionalMetadata_);
    }
    catch (const std::exception& ex) {
        std::ostringstream oss;
        oss << "Encode::encodeField " << ex.what() << " with Message: " << msg;
        std::throw_with_nested(EncodingException(oss.str(), Here()));
    }
}

static ActionBuilder<Encode> EncodeBuilder("encode");

}  // namespace multio::action
