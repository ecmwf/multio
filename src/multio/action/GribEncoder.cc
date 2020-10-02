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
#include <iomanip>
#include <iostream>

#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"
#include "multio/LibMultio.h"
#include "multio/action/GridInfo.h"


namespace multio {
namespace action {

using message::Message;
using message::Peer;

namespace  {
// TODO: perhaps move this to Mappings as that is already a singleton
std::map<std::string, std::unique_ptr<GridInfo>>& grids() {
    static std::map<std::string, std::unique_ptr<GridInfo>> grids_;
    return grids_;
}
}  // namespace

GribEncoder::GribEncoder(codes_handle* handle, const std::string& gridType) :
    metkit::grib::GribHandle{handle}, gridType_{gridType} {
    for (auto const& subtype : {"T grid", "U grid", "V grid", "W grid", "F grid"}) {
        grids().insert(std::make_pair(subtype, std::unique_ptr<GridInfo>{new GridInfo{}}));
    }
}

bool GribEncoder::gridInfoReady(const std::string& subtype) const {
    return grids().at(subtype)->hashExists();
}

bool GribEncoder::setGridInfo(message::Message msg) {
    ASSERT(not gridInfoReady(msg.domain())); // Panic check during development

    ASSERT(coordSet_.find(msg.metadata().getString("nemoParam")) != end(coordSet_));

    grids().at(msg.domain())->setSubtype(msg.domain());

    if (msg.metadata().getString("nemoParam").substr(0, 3) == "lat") {
        grids().at(msg.domain())->setLatitudes(msg);
    }

    if (msg.metadata().getString("nemoParam").substr(0, 3) == "lon") {
        grids().at(msg.domain())->setLongitudes(msg);
    }

    return grids().at(msg.domain())->computeHashIfCan();
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
    setValue("uuidOfHGrid", grids().at(msg.domain())->hashValue());
}

void GribEncoder::setValue(const std::string& key, long value) {
    LOG_DEBUG_LIB(LibMultio) << "*** Setting value " << value << " for key " << key << std::endl;
    CODES_CHECK(codes_set_long(raw(), key.c_str(), value), NULL);
}

void GribEncoder::setValue(const std::string& key, double value) {
    LOG_DEBUG_LIB(LibMultio) << "*** Setting value " << value << " for key " << key << std::endl;
    CODES_CHECK(codes_set_double(raw(), key.c_str(), value), NULL);
}

void GribEncoder::setValue(const std::string& key, const std::string& value) {
    LOG_DEBUG_LIB(LibMultio) << "*** Setting value " << value << " for key " << key << std::endl;
    size_t sz = value.size();
    CODES_CHECK(codes_set_string(raw(), key.c_str(), value.c_str(), &sz), NULL);
}

void GribEncoder::setValue(const std::string& key, const unsigned char* value) {
    std::ostringstream oss;
    for (int i = 0; i < DIGEST_LENGTH; ++i) {
        oss << ((i == 0) ? "" : "-") << std::hex << std::setfill('0') << std::setw(2)
            << static_cast<short>(value[i]);
    }
    LOG_DEBUG_LIB(LibMultio) << "*** Setting value " << oss.str() << " for key " << key
                             << std::endl;
    size_t sz = DIGEST_LENGTH;
    CODES_CHECK(codes_set_bytes(raw(), key.c_str(), value, &sz), NULL);
}

message::Message GribEncoder::encodeLatitudes(const std::string& subtype) {
    auto msg = grids().at(subtype)->latitudes();

    setOceanMetadata(msg);

    return setFieldValues(msg);
}

message::Message GribEncoder::encodeLongitudes(const std::string& subtype) {
    auto msg = grids().at(subtype)->longitudes();

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
