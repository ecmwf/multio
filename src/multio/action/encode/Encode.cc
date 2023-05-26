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

#include "eckit/exception/Exceptions.h"
#include "eckit/io/StdFile.h"
#include "eckit/log/Log.h"

#include "GridDownloader.h"
#include "multio/LibMultio.h"
#include "multio/config/ConfigurationPath.h"
#include "multio/util/ScopedTimer.h"

namespace multio::action {

using config::configuration_path_name;

namespace {

ComponentConfiguration getEncodingConfiguration(const ComponentConfiguration& compConf) {
    if (compConf.parsedConfig().has("encoding")) {
        return ComponentConfiguration(compConf.parsedConfig().getSubConfiguration("encoding"), compConf.multioConfig());
    }
    else {
        return compConf;
    }
}

std::unique_ptr<GribEncoder> make_encoder(const ComponentConfiguration& compConf) {
    auto format = compConf.parsedConfig().getString("format");

    if (format == "grib") {
        ASSERT(compConf.parsedConfig().has("template"));
        std::string tmplPath = compConf.parsedConfig().getString("template");
        // TODO provide utility to distinguish between relative and absolute paths
        eckit::AutoStdFile fin{compConf.multioConfig().replaceCurly(tmplPath)};
        int err;
        return std::make_unique<GribEncoder>(codes_handle_new_from_file(nullptr, fin, PRODUCT_GRIB, &err),
                                             compConf.parsedConfig());
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

Encode::Encode(const ComponentConfiguration& compConf, ComponentConfiguration&& encCompConf) :
    ChainedAction{compConf},
    format_{encCompConf.parsedConfig().getString("format")},
    overwrite_{encCompConf.parsedConfig().has("overwrite")
                   ? std::optional<eckit::LocalConfiguration>{encCompConf.parsedConfig().getSubConfiguration("overwrite")}
                   : std::optional<eckit::LocalConfiguration>{}},
    encoder_{make_encoder(encCompConf)},
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

    if (isOcean(msg.metadata())) {
        //! TODO shoud not be checked here anymore, encoder_ should have been initialized according to format_
        ASSERT(format_ == "grib");

        LOG_DEBUG_LIB(LibMultio) << " *** Looking for grid info for subtype: " << msg.domain() << std::endl;

        const auto& md = msg.metadata();
        auto gridCoords = gridDownloader_->getGridCoords(msg.domain(), md.getInt32("startDate"), md.getInt32("startTime"));
        if (gridCoords) {
            executeNext(gridCoords.value().Lat);
            executeNext(gridCoords.value().Lon);
        }
    }

    auto gridUID = gridDownloader_->getGridUID(msg.domain());
    executeNext(encodeField(std::move(msg), gridUID));
}

void Encode::print(std::ostream& os) const {
    os << "Encode(format=" << format_ << ", "
       << "encoder=";
    if (encoder_)
        encoder_->print(os);
    os << ")";
}

namespace {
message::Metadata applyOverwrites(const eckit::LocalConfiguration& overwrites, message::Metadata md) {
    auto nested = md.getSubConfiguration("encoder-overwrites");
    for (const auto& k : overwrites.keys()) {
        // TODO handle type...
        nested.set(k, overwrites.getString(k));
    }
    md.set("encoder-overwrites", nested);
    return md;
}
}  // namespace

message::Message Encode::encodeField(const message::Message& msg, const std::optional<std::string>& gridUID) const {
    try {
        util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};
        auto md = this->overwrite_ ? applyOverwrites(*this->overwrite_, msg.metadata()) : msg.metadata();
        if (gridUID) {
            md.set("uuidOfHGrid", gridUID.value());
        }
        return encoder_->encodeField(msg.modifyMetadata(std::move(md)));
    }
    catch (...) {
        std::ostringstream oss;
        oss << "Encode::encodeField with Message: " << msg;
        std::throw_with_nested(EncodingException(oss.str(), Here()));
    }
}

static ActionBuilder<Encode> EncodeBuilder("encode");

}  // namespace multio::action
