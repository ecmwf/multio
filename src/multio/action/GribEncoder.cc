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

const std::map<const std::string, const long> ops_to_code{{"instant", 0000},    {"average", 1000},
                                                          {"accumulate", 2000}, {"maximum", 3000},
                                                          {"minimum", 4000},    {"stddev", 5000}};
//  {"average", 0}, {"accumulate", 1}, {"maximum", 2}, {"minimum", 3}, {"stddev", 6}};

const std::map<const std::string, const std::string> category_to_levtype{
    {"ocean-grid-coordinate", "oceanSurface"},
    {"ocean-2d", "oceanSurface"},
    {"ocean-3d", "oceanModelLevel"}};

const std::map<const std::string, const long> type_of_generating_process{
    {"an", 0}, {"in", 1}, {"fc", 2}, {"pf", 4}};

}  // namespace

GribEncoder::GribEncoder(codes_handle* handle, const eckit::LocalConfiguration& config) :
    metkit::grib::GribHandle{handle}, config_{config} {
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

void GribEncoder::setOceanMetadata(const message::Metadata& metadata) {

    // Set run-specific metadata

    setValue("expver", config_.getSubConfiguration("run").getString("expver"));
    setValue("class", config_.getSubConfiguration("run").getString("class"));
    setValue("stream", config_.getSubConfiguration("run").getString("stream"));
    auto type = config_.getSubConfiguration("run").getString("type");
    setValue("type", type);
    setValue("typeOfGeneratingProcess", type_of_generating_process.at(type));

    long date = (type == "an") ? metadata.getLong("currentDate") : metadata.getLong("startDate");
    setValue("year",  date / 10000);
    setValue("month", (date % 10000) / 100);
    setValue("day", date % 100);

    long time = (type == "an") ? metadata.getLong("currentTime") : metadata.getLong("startTime");
    setValue("hour",  time / 10000);
    setValue("minute", (time % 10000) / 100);
    setValue("second", time % 100);

    if (config_.getSubConfiguration("run").has("date-of-analysis")) {
        auto dateOfAnalysis = config_.getSubConfiguration("run").getLong("date-of-analysis");
        setValue("yearOfAnalysis", dateOfAnalysis / 10000);
        setValue("monthOfAnalysis", (dateOfAnalysis % 10000) / 100);
        setValue("dayOfAnalysis", dateOfAnalysis % 100);
    }

    if (config_.getSubConfiguration("run").has("time-of-analysis")) {
        auto timeOfAnalysis = config_.getSubConfiguration("run").getLong("time-of-analysis");
        setValue("hourOfAnalysis", timeOfAnalysis / 10000);
        setValue("minuteOfAnalysis", (timeOfAnalysis % 10000) / 100);
    }

    if (config_.getSubConfiguration("run").has("number")) {
        setValue("number", config_.getSubConfiguration("run").getLong("number"));
    }

    // Statistics field
    if(type == "fc") {
        auto stepInSeconds = metadata.getLong("step") * metadata.getLong("timeStep");
        ASSERT(stepInSeconds % 3600 == 0);
        auto stepInHours = stepInSeconds / 3600;
        auto prevStep = stepInHours - metadata.getLong("timeSpanInHours");
        auto stepRange = std::to_string(prevStep) + "-" + std::to_string(stepInHours);
        if(metadata.getString("operation") == "instant") {
            setValue("step", stepInHours);
        } else {
            setValue("stepRange", stepRange);
        }
    }

    if (metadata.has("operation") and metadata.getString("operation") != "instant") {
        //setValue("typeOfStatisticalProcessing", ops_to_code.at(metadata.getString("operation")));
        // setValue("stepRange", metadata.getString("stepRange"));
        long curDate = metadata.getLong("currentDate");
        setValue("yearOfEndOfOverallTimeInterval",  curDate / 10000);
        setValue("monthOfEndOfOverallTimeInterval", (curDate % 10000) / 100);
        setValue("dayOfEndOfOverallTimeInterval", curDate % 100);
        setValue("lengthOfTimeRange", metadata.getLong("timeSpanInHours"));
        setValue("indicatorOfUnitForTimeIncrement", 13l); // always seconds
        setValue("timeIncrement", metadata.getLong("timeStep"));
    }

    // setDomainDimensions
    setValue("numberOfDataPoints", metadata.getLong("globalSize"));
    setValue("numberOfValues", metadata.getLong("globalSize"));

    // Setting parameter ID
    setValue("paramId",
             metadata.getLong("param") + ops_to_code.at(metadata.getString("operation")));
    setValue("typeOfLevel", metadata.getString("typeOfLevel"));
    if (metadata.getString("category") == "ocean-3d") {
        auto level = metadata.getLong("level");
        ASSERT(level > 0);
        setValue("scaledValueOfFirstFixedSurface", level - 1);
        setValue("scaledValueOfSecondFixedSurface", level);
    }

    // Set ocean grid information
    setValue("unstructuredGridType", config_.getString("grid-type"));

    const auto& gridSubtype = metadata.getString("gridSubtype");
    setValue("unstructuredGridSubtype", gridSubtype.substr(0, 1));

    setValue("uuidOfHGrid", grids().at(gridSubtype)->hashValue());

    // Set encoding for missing value support
    setValue("missingValue", metadata.getDouble("missingValue"));
    setValue("bitmapPresent", static_cast<long>(metadata.getBool("bitmapPresent")));
    setValue("bitsPerValue", metadata.getLong("bitsPerValue"));

    // setValue("missingValue", 0.0);
    // setValue("bitmapPresent", 1l);
    // setValue("bitsPerValue", 16l);
}

void GribEncoder::setCoordMetadata(const message::Metadata& metadata) {
    // Set run-specific metadata
    setValue("expver", config_.getSubConfiguration("run").getString("expver"));
    setValue("class", config_.getSubConfiguration("run").getString("class"));
    setValue("stream", config_.getSubConfiguration("run").getString("stream"));
    const std::string& type = config_.getSubConfiguration("run").getString("type");
    setValue("type", type);
    setValue("typeOfGeneratingProcess", type_of_generating_process.at(type));

    setValue("date", metadata.getLong("startDate"));

    // setDomainDimensions
    setValue("numberOfDataPoints", metadata.getLong("globalSize"));
    setValue("numberOfValues", metadata.getLong("globalSize"));

    // Setting parameter ID
    setValue("paramId", metadata.getLong("param"));

    setValue("typeOfLevel", metadata.getString("typeOfLevel"));

    // Set ocean grid information
    setValue("unstructuredGridType", config_.getString("grid-type"));

    const auto& gridSubtype = metadata.getString("gridSubtype");
    setValue("unstructuredGridSubtype", gridSubtype.substr(0, 1));

    setValue("uuidOfHGrid", grids().at(gridSubtype)->hashValue());

    // Set encoding for missing value support
    setValue("bitmapPresent", static_cast<long>(false));
    setValue("bitsPerValue", metadata.getLong("bitsPerValue"));
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

    setCoordMetadata(msg.metadata());

    return setFieldValues(msg);
}

message::Message GribEncoder::encodeLongitudes(const std::string& subtype) {
    auto msg = grids().at(subtype)->longitudes();

    setCoordMetadata(msg.metadata());

    return setFieldValues(msg);
}

message::Message GribEncoder::encodeField(const message::Message& msg) {
        setOceanMetadata(msg.metadata());
        return setFieldValues(msg);
}

message::Message GribEncoder::encodeField(const message::Metadata& md, const double* data,
                                          size_t sz) {
    setOceanMetadata(md);
    return setFieldValues(data, sz);
}

message::Message GribEncoder::setFieldValues(const message::Message& msg) {
    auto beg = reinterpret_cast<const double*>(msg.payload().data());
    this->setDataValues(beg, msg.globalSize());

    eckit::Buffer buf{this->length()};
    this->write(buf);

    return Message{Message::Header{Message::Tag::Grib, Peer{msg.source().group()}, Peer{msg.destination()}}, std::move(buf)};
}

message::Message GribEncoder::setFieldValues(const double* values, size_t count) {
    this->setDataValues(values, count);

    eckit::Buffer buf{this->length()};
    this->write(buf);

    return Message{Message::Header{Message::Tag::Grib, Peer{}, Peer{}}, std::move(buf)};
}

}  // namespace action
}  // namespace multio
