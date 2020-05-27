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

#include "metkit/grib/GribDataBlob.h"

#include "multio/LibMultio.h"
#include "multio/server/ConfigurationPath.h"

namespace multio {
namespace action {

GribEncoder::GribEncoder(codes_handle* handle) : metkit::grib::GribHandle{handle} {}

void GribEncoder::setOceanValues(const message::Metadata& md) {
    // setCommonMetadata
    setValue("expver", "xxxx");
    setValue("class", "rd");
    setValue("stream", "oper");
    setValue("type", "fc");
    setValue("levtype", static_cast<long>(168));
    setValue("step", md.getLong("step"));
    setValue("level", md.getLong("level"));

    // setDomainDimensions
    setValue("numberOfDataPoints", md.getLong("globalSize"));
    setValue("numberOfValues", md.getLong("globalSize"));

    // Setting parameter ID
    setValue("param", md.getLong("param"));

}

void GribEncoder::setValue(const std::string& key, long value) {
    eckit::Log::debug<multio::LibMultio>() << "Setting value for key " << key << std::endl;
    CODES_CHECK(codes_set_long(raw(), key.c_str(), value), NULL);
}

void GribEncoder::setValue(const std::string& key, double value) {
    eckit::Log::debug<multio::LibMultio>() << "Setting value for key " << key << std::endl;
    CODES_CHECK(codes_set_double(raw(), key.c_str(), value), NULL);
}

void GribEncoder::setValue(const std::string& key, const std::string& value) {
    eckit::Log::debug<multio::LibMultio>() << "Setting value for key " << key << std::endl;
    size_t sz = value.size();
    CODES_CHECK(codes_set_string(raw(), key.c_str(), value.c_str(), &sz), NULL);
}

namespace {

std::unique_ptr<GribEncoder> make_encoder(const eckit::Configuration& config) {
    auto format = config.getString("format");

    if (format == "grib") {
        ASSERT(config.has("template"));
        eckit::AutoStdFile fin{configuration_path() + config.getString("template")};
        int err;
        return std::unique_ptr<GribEncoder>{
            new GribEncoder{codes_handle_new_from_file(nullptr, fin, PRODUCT_GRIB, &err)}};
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

bool Encode::doExecute(Message& msg) const {
    ScopedTimer timer{timing_};

    eckit::Log::debug<LibMultio>() << "*** Executing encoding: " << *this << std::endl;

    if (not encoder_) {
        return true;
    }

    ASSERT(format_ == "grib");

    encoder_->setOceanValues(msg.metadata());

    // Setting field values
    auto beg = reinterpret_cast<const double*>(msg.payload().data());
    encoder_->setDataValues(beg, msg.globalSize());

    eckit::Buffer buf{encoder_->message()->length()};
    encoder_->write(buf);
    msg = Message{Message::Header{Message::Tag::Grib, Peer{"", 0}, Peer{"", 0}}, std::move(buf)};

    return true;
}

void Encode::print(std::ostream& os) const {
    os << "Encode(format=" << format_ << ")";
}

static ActionBuilder<Encode> EncodeBuilder("Encode");

}  // namespace action
}  // namespace multio
