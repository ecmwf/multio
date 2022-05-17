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
#include "multio/server/ConfigurationPath.h"
#include "multio/util/ScopedTimer.h"

namespace multio {
namespace action {

namespace {

std::unique_ptr<GribEncoder> make_encoder(const eckit::Configuration& config) {
    auto format = config.getString("format");

    if (format == "grib") {
        ASSERT(config.has("template"));
        eckit::AutoStdFile fin{configuration_path() + config.getString("template")};
        int err;
        return std::unique_ptr<GribEncoder>{
            new GribEncoder{codes_handle_new_from_file(nullptr, fin, PRODUCT_GRIB, &err),
                            config.getString("grid-type", "ORCA1")}};
    }
    else if (format == "none") {
        return nullptr;  // leave message in raw binary format
    }
    else {
        throw eckit::SeriousBug("Encoding format <" + format + "> is not supported");
    }
}
}  // namespace

using message::Message;
using message::Peer;

Encode::Encode(const eckit::Configuration& config) :
    Action{config}, format_{config.getString("format")}, encoder_{make_encoder(config)} {}

void Encode::execute(Message msg) const {
    if (not encoder_) {
        executeNext(msg);
        return;
    }

    ASSERT(format_ == "grib");

    LOG_DEBUG_LIB(LibMultio) << " *** Looking for grid info for subtype: " << msg.domain()
                             << std::endl;

    if (encoder_->gridInfoReady(msg.domain())) {
        auto levelCount = msg.metadata().getLong("levelCount", 1);
        ASSERT(levelCount == 1);
        // TODO: most of this can probably go if we stick to levelCount == 1 always
        if (levelCount == 1) {
            executeNext(encoder_->encodeField(msg));
        }
        else { // TODO: this branch can probably go. See above...
            auto metadata = msg.metadata();
            auto data = reinterpret_cast<const double*>(msg.payload().data());
            for (auto lev = 0; lev != levelCount;) {
                metadata.set("level", ++lev);
                executeNext(encoder_->encodeField(metadata, data, msg.globalSize()));
                data += msg.globalSize();
            }
        }
    }
    else {
        LOG_DEBUG_LIB(LibMultio) << "*** Grid metadata: " << msg.metadata() << std::endl;
        if (encoder_->setGridInfo(msg)) {
            executeNext(encodeLatitudes(msg.domain()));
            executeNext(encodeLongitudes(msg.domain()));
        }
    }
}

void Encode::print(std::ostream& os) const {
    os << "Encode(format=" << format_ << ")";
}

message::Message Encode::encodeField(const message::Message& msg) const {
    util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};
    return encoder_->encodeField(msg);
}

message::Message Encode::encodeLatitudes(const std::string& subtype) const {
    util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};
    return encoder_->encodeLatitudes(subtype);
}

message::Message Encode::encodeLongitudes(const std::string& subtype) const {
    util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};
    return encoder_->encodeLongitudes(subtype);
}

static ActionBuilder<Encode> EncodeBuilder("Encode");

}  // namespace action
}  // namespace multio
