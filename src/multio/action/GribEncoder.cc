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
#include "multio/LibMultio.h"

namespace multio {
namespace action {

using message::Message;
using message::Peer;

GribEncoder::GribEncoder(codes_handle* handle, const std::string& gridType) :
    metkit::grib::GribHandle{handle}, gridType_{gridType} {
    for (auto const& subtype : {"T grid", "U grid", "V grid", "W grid", "F grid"}) {
        grids_.insert(std::make_pair(subtype, std::unique_ptr<GridInfo>{new GridInfo{}}));
    }
}

bool GribEncoder::gridInfoReady(const std::string& subtype) const {
    return grids_.at(subtype)->hashExists();
}

bool GribEncoder::setGridInfo(message::Message msg) {
    ASSERT(not gridInfoReady(msg.domain())); // Panic check during development

    ASSERT(coordSet_.find(msg.metadata().getString("nemoParam")) != end(coordSet_));

    grids_.at(msg.domain())->setSubtype(msg.domain());

    if (msg.metadata().getString("nemoParam").substr(0, 3) == "lat") {
        grids_.at(msg.domain())->setLatitudes(msg);
    }

    if (msg.metadata().getString("nemoParam").substr(0, 3) == "lon") {
        grids_.at(msg.domain())->setLongitudes(msg);
    }

    return grids_.at(msg.domain())->computeHashIfCan();
}

void GribEncoder::setOceanMetadata(const message::Message& msg) {
    // setCommonMetadata
    setValue("expver", "xxxx");
    setValue("class", "rd");
    setValue("stream", "oper");
    setValue("type", "fc");
    setValue("levtype", static_cast<long>(168));
    setValue("step", msg.metadata().getLong("step"));
    setValue("level", msg.metadata().getLong("level"));

    // TODO: Nemo should set this at the beginning of the run
    setValue("date", 20170906l);

    // setDomainDimensions
    setValue("numberOfDataPoints", msg.metadata().getLong("globalSize"));
    setValue("numberOfValues", msg.metadata().getLong("globalSize"));

    // Setting parameter ID
    setValue("paramId", msg.metadata().getLong("param"));

    // Set ocean grid information
    setValue("unstructuredGridType", gridType_);
    setValue("unstructuredGridSubtype", msg.domain());
    setValue("uuidOfHGrid", grids_.at(msg.domain())->hashValue());
}

void GribEncoder::setValue(const std::string& key, long value) {
    eckit::Log::info()
        << "*** Setting value " << value << " for key " << key << std::endl;
    CODES_CHECK(codes_set_long(raw(), key.c_str(), value), NULL);
}

void GribEncoder::setValue(const std::string& key, double value) {
    eckit::Log::info()
        << "*** Setting value " << value << " for key " << key << std::endl;
    CODES_CHECK(codes_set_double(raw(), key.c_str(), value), NULL);
}

void GribEncoder::setValue(const std::string& key, const std::string& value) {
    eckit::Log::info()
        << "*** Setting value " << value << " for key " << key << std::endl;
    size_t sz = value.size();
    CODES_CHECK(codes_set_string(raw(), key.c_str(), value.c_str(), &sz), NULL);
}

void GribEncoder::setValue(const std::string& key, const unsigned char* value) {
    eckit::Log::info()
        << "*** Setting value " << value << " for key " << key << std::endl;
    size_t sz = DIGEST_LENGTH;
    eckit::Log::info() << "*** Setting value " << value << " with size " << sz << " for key " << key
                       << std::endl;
    CODES_CHECK(codes_set_bytes(raw(), key.c_str(), value, &sz), NULL);
}

message::Message GribEncoder::encodeLatitudes(const std::string& subtype) {
    auto msg = grids_.at(subtype)->latitudes();

    setOceanMetadata(msg);

    return setFieldValues(msg);
}

message::Message GribEncoder::encodeLongitudes(const std::string& subtype) {
    auto msg = grids_.at(subtype)->longitudes();

    setOceanMetadata(msg);

    return setFieldValues(msg);
}

message::Message GribEncoder::encodeField(const message::Message& msg) {
    setOceanMetadata(msg);

    return setFieldValues(msg);
}

message::Message GribEncoder::setFieldValues(const message::Message& msg) {
    auto beg = reinterpret_cast<const double*>(msg.payload().data());
    this->setDataValues(beg, msg.globalSize());

    eckit::Buffer buf{this->length()};
    this->write(buf);

    return Message{Message::Header{Message::Tag::Grib, Peer{"", 0}, Peer{"", 0}}, std::move(buf)};
}

}  // namespace action
}  // namespace multio
