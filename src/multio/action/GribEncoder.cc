/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany

/// @date Aug 2020

#include "GribEncoder.h"

#include <cstring>
#include <iostream>

#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"
#include "metkit/grib/GribDataBlob.h"
#include "multio/LibMultio.h"

namespace multio {
namespace action {

using message::Message;
using message::Peer;

GribEncoder::GribEncoder(codes_handle* handle) : metkit::grib::GribHandle{handle} {}

bool GribEncoder::gridInfoReady(const std::string& subtype) const {
    return grids_.at(subtype).hash();
}

bool GribEncoder::setGridInfo(message::Message msg) {
    ASSERT(not grids_.at(msg.domain()).hash()); // Panic check during development

    grids_.at(msg.domain()).setSubtype(msg.domain());

    if (msg.metadata().getString("nemoParam").substr(0, 3) == "lat") {
        grids_.at(msg.domain()).setLatitudes(msg);
    }

    if (msg.metadata().getString("nemoParam").substr(0, 3) == "lon") {
        grids_.at(msg.domain()).setLongitudes(msg);
    }

    return grids_.at(msg.domain()).computeHashIfCan();
}

void GribEncoder::setOceanValues(const message::Metadata& md, const std::string& subtype) {
    // setCommonMetadata
    setValue("expver", "xxxx");
    setValue("class", "rd");
    setValue("stream", "oper");
    setValue("type", "fc");
    setValue("levtype", static_cast<long>(168));
    setValue("step", md.getLong("step"));
    setValue("level", md.getLong("level"));

    // TODO: Nemo should set this at the beginning of the run
    setValue("date", 20170906l);

    // setDomainDimensions
    setValue("numberOfDataPoints", md.getLong("globalSize"));
    setValue("numberOfValues", md.getLong("globalSize"));

    // Setting parameter ID
    setValue("param", md.getLong("param"));

    setValue("uuidOfHGrid", grids_.at(subtype).hash());
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
    eckit::Log::debug<multio::LibMultio>()
        << "Setting value " << value << " for key " << key << std::endl;
    size_t sz = value.size();
    CODES_CHECK(codes_set_string(raw(), key.c_str(), value.c_str(), &sz), NULL);
}

void GribEncoder::setValue(const std::string& key, const unsigned char* value) {
    eckit::Log::debug<multio::LibMultio>()
        << "Setting value " << value << " for key " << key << std::endl;
    size_t sz = std::strlen(reinterpret_cast<const char*>(value));
    CODES_CHECK(codes_set_bytes(raw(), key.c_str(), value, &sz), NULL);
}

message::Message GribEncoder::encodeLatitudes(const std::string& subtype) {
    auto msg = grids_.at(subtype).latitudes();

    setCoordinateMetadata(msg.metadata());

    setValue("uuidOfHGrid", grids_.at(subtype).hash());

    // Setting field values -- common to all fields, extract it
    auto beg = reinterpret_cast<const double*>(msg.payload().data());
    this->setDataValues(beg, msg.globalSize());

    eckit::Buffer buf{this->message()->length()};
    this->write(buf);

    return Message{Message::Header{Message::Tag::Grib, Peer{"", 0}, Peer{"", 0}}, std::move(buf)};
}

message::Message GribEncoder::encodeLongitudes(const std::string& subtype) {
    auto msg = grids_.at(subtype).longitudes();

    setCoordinateMetadata(msg.metadata());

    setValue("uuidOfHGrid", grids_.at(subtype).hash());

    // Setting field values -- common to all fields, extract it
    auto beg = reinterpret_cast<const double*>(msg.payload().data());
    this->setDataValues(beg, msg.globalSize());

    eckit::Buffer buf{this->message()->length()};
    this->write(buf);

    return Message{Message::Header{Message::Tag::Grib, Peer{"", 0}, Peer{"", 0}}, std::move(buf)};
}

void GribEncoder::setCoordinateMetadata(const message::Metadata& md) {
    setValue("expver", "xxxx");
    setValue("class", "rd");
    setValue("stream", "oper");
    setValue("type", "fc");

    // TODO: Nemo should set this at the beginning of the run
    setValue("date", 20170906l);

    // setDomainDimensions
    setValue("numberOfDataPoints", md.getLong("globalSize"));
    setValue("numberOfValues", md.getLong("globalSize"));

    // Setting parameter ID
    setValue("param", md.getLong("param"));
}

}  // namespace action
}  // namespace multio
