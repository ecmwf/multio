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

#include "multio/LibMultio.h"
#include "multio/util/ConfigurationPath.h"
#include "multio/util/ScopedTimer.h"

namespace multio {
namespace action {

using util::configuration_path_name;

namespace {

ConfigurationContext getEncodingConfiguration(const ConfigurationContext& confCtx) {
    if (confCtx.config().has("encoding")) {
        return confCtx.recast(confCtx.config().getSubConfiguration("encoding"));
    }
    else {
        return confCtx;
    }
}

std::unique_ptr<GribEncoder> make_encoder(const ConfigurationContext& confCtx) {
    auto format = confCtx.config().getString("format");

    if (format == "grib") {
        ASSERT(confCtx.config().has("template"));
        std::string tmplPath = confCtx.config().getString("template");
        // TODO provide utility to distinguish between relative and absolute paths
        eckit::AutoStdFile fin{confCtx.replaceCurly(tmplPath)};
        int err;
        return std::make_unique<GribEncoder>(codes_handle_new_from_file(nullptr, fin, PRODUCT_GRIB, &err), confCtx.config());
    }
    else if (format == "raw") {
        return nullptr;  // leave message in raw binary format
    }
    else {
        throw eckit::SeriousBug("Encoding format <" + format + "> is not supported");
    }
}

}  // namespace

using message::Message;
using message::Peer;

Encode::Encode(const ConfigurationContext& confCtx, ConfigurationContext&& encConfCtx) :
    ChainedAction{confCtx},
    format_{encConfCtx.config().getString("format")},
    overwrite_{encConfCtx.config().has("overwrite")
                   ? eckit::Optional<eckit::LocalConfiguration>{encConfCtx.config().getSubConfiguration("overwrite")}
                   : eckit::Optional<eckit::LocalConfiguration>{}},
    encoder_{make_encoder(encConfCtx)} {}

Encode::Encode(const ConfigurationContext& confCtx) : Encode(confCtx, getEncodingConfiguration(confCtx)) {}

void Encode::executeImpl(Message msg) const {
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

        if (encoder_->gridInfoReady(msg.domain())) {
            executeNext(encodeField(std::move(msg)));
        }
        else {
            LOG_DEBUG_LIB(LibMultio) << "*** Grid metadata: " << msg.metadata() << std::endl;
            if (encoder_->setGridInfo(msg)) {
                executeNext(encodeOceanLatitudes(msg.domain()));
                executeNext(encodeOceanLongitudes(msg.domain()));
            }
        }
    }
    else {
        executeNext(encodeField(std::move(msg)));
    }
}

void Encode::print(std::ostream& os) const {
    os << "Encode(format=" << format_ << ")";
}

namespace {
message::Metadata applyOverwrites(const eckit::LocalConfiguration& overwrites, message::Metadata md) {
    auto nested = md.getSubConfiguration("encoderOverwrites");
    for (const auto& k : overwrites.keys()) {
        // TODO handle type...
        nested.set(k, overwrites.getString(k));
    }
    md.set("encoderOverwrites", nested);
    return md;
}
}  // namespace

message::Message Encode::encodeField(const message::Message& msg) const {
    try {
        util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};
        return encoder_->encodeField(
            this->overwrite_ ? msg.modifyMetadata(applyOverwrites(*this->overwrite_, msg.metadata())) : msg);
    }
    catch (...) {
        std::ostringstream oss;
        oss << "Encode::encodeField with Message: " << msg;
        std::throw_with_nested(eckit::Exception(oss.str()));
    }
}

message::Message Encode::encodeOceanLatitudes(const std::string& subtype) const {
    try {
        util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};
        return encoder_->encodeOceanLatitudes(subtype);
    }
    catch (...) {
        std::ostringstream oss;
        oss << "Encode::encodeOceanLatitudes with subtype: " << subtype;
        std::throw_with_nested(eckit::Exception(oss.str()));
    }
}

message::Message Encode::encodeOceanLongitudes(const std::string& subtype) const {
    try {
        util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};
        return encoder_->encodeOceanLongitudes(subtype);
    }
    catch (...) {
        std::ostringstream oss;
        oss << "Encode::encodeOceanLongitudes with subtype: " << subtype;
        std::throw_with_nested(eckit::Exception(oss.str()));
    }
}

static ActionBuilder<Encode> EncodeBuilder("encode");

}  // namespace action
}  // namespace multio
