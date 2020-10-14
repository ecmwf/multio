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
    ScopedTimer timer{timing_};

    eckit::Log::info() << "*** Executing encoding: " << *this << std::endl;

    if (not encoder_) {
        executeNext(msg);
        return;
    }

    ASSERT(format_ == "grib");

    if (encoder_->gridInfoReady(msg.domain())) {
        executeNext(encoder_->encodeField(msg));
    }
    else {
        eckit::Log::info() << "*** Grid metadata: " << msg.metadata() << std::endl;
        if (encoder_->setGridInfo(msg)) {
            executeNext(encoder_->encodeLatitudes(msg.domain()));
            executeNext(encoder_->encodeLongitudes(msg.domain()));
        }
    }
}

void Encode::print(std::ostream& os) const {
    os << "Encode(format=" << format_ << ")";
}

static ActionBuilder<Encode> EncodeBuilder("Encode");

}  // namespace action
}  // namespace multio
