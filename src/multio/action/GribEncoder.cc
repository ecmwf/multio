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
#include "multio/util/Metadata.h"


namespace multio {
namespace action {

using message::Message;
using message::Peer;

using util::firstOf;
using util::LookUpBool;
using util::lookUpBool;
using util::LookUpDouble;
using util::lookUpDouble;
using util::LookUpLong;
using util::lookUpLong;
using util::LookUpString;
using util::lookUpString;
using util::withFirstOf;

namespace {
// TODO: perhaps move this to Mappings as that is already a singleton
std::map<std::string, std::unique_ptr<GridInfo>>& grids() {
    static std::map<std::string, std::unique_ptr<GridInfo>> grids_;
    return grids_;
}

const std::map<const std::string, const long> ops_to_code{{"instant", 0000}, {"average", 1000}, {"accumulate", 2000},
                                                          {"maximum", 3000}, {"minimum", 4000}, {"stddev", 5000}};
//  {"average", 0}, {"accumulate", 1}, {"maximum", 2}, {"minimum", 3}, {"stddev", 6}};

const std::map<const std::string, const std::string> category_to_levtype{
    {"ocean-grid-coordinate", "oceanSurface"}, {"ocean-2d", "oceanSurface"}, {"ocean-3d", "oceanModelLevel"}};

const std::map<const std::string, const long> type_of_generating_process{{"an", 0}, {"in", 1}, {"fc", 2}, {"pf", 4}};


struct ValueSetter {
    GribEncoder& g_;
    std::string key_;

    template <typename T>
    void operator()(T&& t) {
        g_.setValue(key_, std::forward<T>(t));
    }
};

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
    ASSERT(not gridInfoReady(msg.domain()));  // Panic check during development

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


eckit::Optional<std::string> setMarsFields(GribEncoder& g, const eckit::Configuration& md) {
    withFirstOf(ValueSetter{g, "domain"}, LookUpString(md, "domain"), LookUpString(md, "globalDomain"));
    withFirstOf(ValueSetter{g, "levtype"}, LookUpString(md, "levtype"), LookUpString(md, "indicatorOfTypeOfLevel"));
    withFirstOf(ValueSetter{g, "level"}, LookUpLong(md, "level"), LookUpLong(md, "levelist"));
    withFirstOf(ValueSetter{g, "date"}, LookUpLong(md, "date"), LookUpLong(md, "dataDate"));
    withFirstOf(ValueSetter{g, "time"}, LookUpLong(md, "time"), LookUpLong(md, "dataTime"));
    withFirstOf(ValueSetter{g, "step"}, LookUpLong(md, "step"), LookUpLong(md, "startStep"));
    withFirstOf(ValueSetter{g, "param"}, LookUpLong(md, "param"), LookUpLong(md, "indicatorOfParameter"));
    withFirstOf(ValueSetter{g, "class"}, LookUpString(md, "class"), LookUpString(md, "marsClass"));
    withFirstOf(ValueSetter{g, "stream"}, LookUpString(md, "stream"), LookUpString(md, "marsStream"));
    withFirstOf(ValueSetter{g, "expver"}, LookUpString(md, "expver"), LookUpString(md, "experimentVersionNumber"));

    auto type = firstOf(LookUpString(md, "type"), LookUpString(md, "marsType"));
    if (type) {
        g.setValue("type", *type);
        g.setValue("typeOfGeneratingProcess", type_of_generating_process.at(*type));
    }
    return type;
}

void setEncodingSpecificFields(GribEncoder& g, const eckit::Configuration& md) {
    // globalSize is expected to be set in md directly
    auto gls = lookUpLong(md, "globalSize");
    withFirstOf(ValueSetter{g, "numberOfDataPoints"}, gls);
    withFirstOf(ValueSetter{g, "numberOfValues"}, gls);

    withFirstOf(ValueSetter{g, "missingValue"}, LookUpString(md, "missingValue"));
    withFirstOf(ValueSetter{g, "bitmapPresent"}, LookUpBool(md, "bitmapPresent"));
    withFirstOf(ValueSetter{g, "bitsPerValue"}, firstOf(LookUpLong(md, "bitsPerValue")));
}

void setDateAndStatisticalFields(GribEncoder& g, const eckit::Configuration& md,
                                 const eckit::Optional<std::string>& type) {
    auto date
        = firstOf(LookUpLong(md, (type && (*type == "an")) ? "currentDate" : "startDate"), LookUpLong(md, "startDate"));
    if (date) {
        g.setValue("year", *date / 10000);
        g.setValue("month", (*date % 10000) / 100);
        g.setValue("day", *date % 100);
    }
    auto time
        = firstOf(LookUpLong(md, (type && (*type == "an")) ? "currentTime" : "startTime"), LookUpLong(md, "startTime"));
    if (time) {
        g.setValue("hour", *time / 10000);
        g.setValue("minute", (*time % 10000) / 100);
        g.setValue("second", *time % 100);
    }

    auto dateOfAnalysis = firstOf(LookUpLong(md, "date-of-analysis"));
    if (dateOfAnalysis) {
        g.setValue("yearOfAnalysis", *dateOfAnalysis / 10000);
        g.setValue("monthOfAnalysis", (*dateOfAnalysis % 10000) / 100);
        g.setValue("dayOfAnalysis", *dateOfAnalysis % 100);
    }

    auto timeOfAnalysis = firstOf(LookUpLong(md, "time-of-analysis"));
    if (timeOfAnalysis) {
        g.setValue("hourOfAnalysis", *timeOfAnalysis / 10000);
        g.setValue("minuteOfAnalysis", (*timeOfAnalysis % 10000) / 100);
    }

    withFirstOf(ValueSetter{g, "number"}, LookUpLong(md, "number"));

    auto operation = lookUpString(md, "operation");
    if (operation) {
        // Statistics field
        if (*type == "fc" && *operation == "instant") {
            withFirstOf(ValueSetter{g, "step"}, LookUpLong(md, "stepInHours"));
        }
        else {
            withFirstOf(ValueSetter{g, "stepRange"}, LookUpLong(md, "stepRangeInHours"));
        }

        eckit::Optional<long> curDate;
        if (*operation != "instant" && (curDate = firstOf(LookUpLong(md, "currentDate")))) {
            // g.setValue("typeOfStatisticalProcessing", ops_to_code.at(metadata.getString("operation")));
            //  g.setValue("stepRange", metadata.getString("stepRange"));
            g.setValue("yearOfEndOfOverallTimeInterval", *curDate / 10000);
            g.setValue("monthOfEndOfOverallTimeInterval", (*curDate % 10000) / 100);
            g.setValue("dayOfEndOfOverallTimeInterval", *curDate % 100);
            g.setValue("lengthOfTimeRange", md.getLong("timeSpanInHours"));
            g.setValue("indicatorOfUnitForTimeIncrement", 13l);  // always seconds
            g.setValue("timeIncrement", md.getLong("timeStep"));
        }
    }
}

void GribEncoder::setFieldMetadata(const message::Metadata& metadata) {
    if (metadata.has("encodingKeys")) {
        auto runConfig = metadata.getSubConfiguration("encodingKeys");

        auto type = setMarsFields(*this, runConfig);
        setEncodingSpecificFields(*this, runConfig);
        setDateAndStatisticalFields(*this, runConfig, type);
    }
    else if (isOcean(metadata)) {
        setOceanMetadata(metadata);
    }
}

void GribEncoder::setOceanMetadata(const message::Metadata& metadata) {
    auto runConfig = config_.getSubConfiguration("run");

    auto typeMaybe = setMarsFields(*this, runConfig);
    setDateAndStatisticalFields(*this, runConfig, typeMaybe);
    setEncodingSpecificFields(*this, metadata);

    // Setting parameter ID
    setValue("paramId", metadata.getLong("param") + ops_to_code.at(metadata.getString("operation")));
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
}

void GribEncoder::setCoordMetadata(const message::Metadata& metadata) {
    setCoordMetadata(metadata, metadata.has("encodingKeys") ? metadata.getSubConfiguration("encodingKeys")
                                                            : config_.getSubConfiguration("run"));
}
void GribEncoder::setCoordMetadata(const message::Metadata& md, const eckit::Configuration& runConfig) {

    // Set run-specific md
    setMarsFields(*this, runConfig);
    
    setValue("date", md.getLong("startDate"));

    // setDomainDimensions
    auto gls = lookUpLong(md, "globalSize");
    setValue("numberOfDataPoints", md.getLong("globalSize"));
    setValue("numberOfValues", md.getLong("globalSize"));

    // Setting parameter ID
    setValue("paramId", md.getLong("param"));

    setValue("typeOfLevel", md.getString("typeOfLevel"));

    // Set ocean grid information
    setValue("unstructuredGridType", config_.getString("grid-type"));

    const auto& gridSubtype = md.getString("gridSubtype");
    setValue("unstructuredGridSubtype", gridSubtype.substr(0, 1));

    setValue("uuidOfHGrid", grids().at(gridSubtype)->hashValue());

    // Set encoding for missing value support
    setValue("bitmapPresent", static_cast<long>(false));
    setValue("bitsPerValue", md.getLong("bitsPerValue"));
}

void codesCheck(int ret) {
    if (ret == CODES_READ_ONLY) {
        // If value is read only, do not panic...
        return;
    }
    CODES_CHECK(ret, NULL);
}

void GribEncoder::setValue(const std::string& key, long value) {
    LOG_DEBUG_LIB(LibMultio) << "*** Setting value " << value << " for key " << key << std::endl;
    codesCheck(codes_set_long(raw(), key.c_str(), value));
}

void GribEncoder::setValue(const std::string& key, double value) {
    LOG_DEBUG_LIB(LibMultio) << "*** Setting value " << value << " for key " << key << std::endl;
    codesCheck(codes_set_double(raw(), key.c_str(), value));
}

void GribEncoder::setValue(const std::string& key, const std::string& value) {
    LOG_DEBUG_LIB(LibMultio) << "*** Setting value " << value << " for key " << key << std::endl;
    size_t sz = value.size();
    codesCheck(codes_set_string(raw(), key.c_str(), value.c_str(), &sz));
}

void GribEncoder::setValue(const std::string& key, const unsigned char* value) {
    std::ostringstream oss;
    for (int i = 0; i < DIGEST_LENGTH; ++i) {
        oss << ((i == 0) ? "" : "-") << std::hex << std::setfill('0') << std::setw(2) << static_cast<short>(value[i]);
    }
    LOG_DEBUG_LIB(LibMultio) << "*** Setting value " << oss.str() << " for key " << key << std::endl;
    size_t sz = DIGEST_LENGTH;
    codesCheck(codes_set_bytes(raw(), key.c_str(), value, &sz));
}

void GribEncoder::setValue(const std::string& key, bool value) {
    LOG_DEBUG_LIB(LibMultio) << "*** Setting value " << value << "(" << static_cast<long>(value) << ") for key " << key
                             << std::endl;
    codesCheck(codes_set_long(raw(), key.c_str(), static_cast<long>(value)));
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
    setFieldMetadata(msg.metadata());
    return setFieldValues(msg);
}

message::Message GribEncoder::encodeField(const message::Metadata& md, const double* data, size_t sz) {
    setFieldMetadata(md);
    return setFieldValues(data, sz);
}

message::Message GribEncoder::setFieldValues(const message::Message& msg) {
    auto beg = reinterpret_cast<const double*>(msg.payload().data());
    // TODO refactor
    // this->setDataValues(beg, msg.metadata().has("globalSize") ? msg.globalSize() : msg.payload().size() /
    // sizeof(double));
    this->setDataValues(beg, msg.globalSize());

    eckit::Buffer buf{this->length()};
    this->write(buf);

    return Message{Message::Header{Message::Tag::Grib, Peer{msg.source().group()}, Peer{msg.destination()}},
                   std::move(buf)};
}

message::Message GribEncoder::setFieldValues(const double* values, size_t count) {
    this->setDataValues(values, count);

    eckit::Buffer buf{this->length()};
    this->write(buf);

    return Message{Message::Header{Message::Tag::Grib, Peer{}, Peer{}}, std::move(buf)};
}

}  // namespace action
}  // namespace multio
