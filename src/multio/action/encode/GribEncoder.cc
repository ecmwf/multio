/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany

/// @date Aug 2020

#include "GribEncoder.h"

#include <cstring>
#include <iomanip>
#include <iostream>

#include "GridInfo.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"
#include "multio/LibMultio.h"
#include "multio/util/Metadata.h"


#include "multio/util/PrecisionTag.h"

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

const std::map<const std::string, const long> type_of_statistical_processing{
    {"average", 0}, {"accumulate", 1}, {"maximum", 2}, {"minimum", 3}, {"stddev", 6}};

const std::map<const std::string, const std::string> category_to_levtype{
    {"ocean-grid-coordinate", "oceanSurface"}, {"ocean-2d", "oceanSurface"}, {"ocean-3d", "oceanModelLevel"}};

const std::map<const std::string, const long> type_of_generating_process{
    {"an", 0}, {"in", 1}, {"fc", 2}, {"pf", 4}, {"tpa", 12}};

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
    metkit::grib::GribHandle{handle}, config_{config} /*, encodeBitsPerValue_(config)*/ {
    for (auto const& subtype : {"T grid", "U grid", "V grid", "W grid", "F grid"}) {
        grids().insert(std::make_pair(subtype, std::make_unique<GridInfo>()));
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

struct QueriedMarsKeys {
    eckit::Optional<std::string> type{};
    eckit::Optional<long> paramId{};
};

QueriedMarsKeys setMarsKeys(GribEncoder& g, const eckit::Configuration& md) {
    QueriedMarsKeys ret;
    // TODO we should be able to determine the type in the metadata and preserve
    // it Domain usually is always readonly withFirstOf(ValueSetter{g, "domain"},
    // LookUpString(md, "domain"), LookUpString(md, "globalDomain"));
    withFirstOf(ValueSetter{g, "levtype"}, LookUpString(md, "levtype"), LookUpString(md, "indicatorOfTypeOfLevel"));
    withFirstOf(ValueSetter{g, "level"}, LookUpLong(md, "level"), LookUpLong(md, "levelist"));
    withFirstOf(ValueSetter{g, "date"}, LookUpLong(md, "date"), LookUpLong(md, "dataDate"));
    withFirstOf(ValueSetter{g, "time"}, LookUpLong(md, "time"), LookUpLong(md, "dataTime"));
    withFirstOf(ValueSetter{g, "step"}, LookUpLong(md, "step"), LookUpLong(md, "startStep"));

    ret.paramId = firstOf(LookUpLong(md, "paramId"),
                          LookUpLong(md, "param"));  // param might be a string, separated by . for GRIB1.
                                                     // String to long convertion should get it right
    if (ret.paramId) {
        g.setValue("paramId", *ret.paramId);
    }
    withFirstOf(ValueSetter{g, "class"}, LookUpString(md, "class"), LookUpString(md, "marsClass"));
    withFirstOf(ValueSetter{g, "stream"}, LookUpString(md, "stream"), LookUpString(md, "marsStream"));
    withFirstOf(ValueSetter{g, "expver"}, LookUpString(md, "expver"), LookUpString(md, "experimentVersionNumber"));

    ret.type = firstOf(LookUpString(md, "type"), LookUpString(md, "marsType"));
    if (ret.type) {
        g.setValue("type", *ret.type);
    }

    // Additional parameters passed through for spherical harmonics
    if (md.has("gridType") && md.getString("gridType") == "sh") {
        withFirstOf(ValueSetter{g, "complexPacking"}, LookUpLong(md, "complexPacking"));
        withFirstOf(ValueSetter{g, "pentagonalResolutionParameterJ"}, LookUpLong(md, "pentagonalResolutionParameterJ"),
                    LookUpLong(md, "J"));
        withFirstOf(ValueSetter{g, "pentagonalResolutionParameterK"}, LookUpLong(md, "pentagonalResolutionParameterK"),
                    LookUpLong(md, "K"));
        withFirstOf(ValueSetter{g, "pentagonalResolutionParameterM"}, LookUpLong(md, "pentagonalResolutionParameterM"),
                    LookUpLong(md, "M"));
        // withFirstOf(ValueSetter{g, "unpackedSubsetPrecision"}, LookUpLong(md,
        // "unpackedSubsetPrecision"));
        withFirstOf(ValueSetter{g, "subSetJ"}, LookUpLong(md, "subSetJ"), LookUpLong(md, "JS"));
        withFirstOf(ValueSetter{g, "subSetK"}, LookUpLong(md, "subSetK"), LookUpLong(md, "KS"));
        withFirstOf(ValueSetter{g, "subSetM"}, LookUpLong(md, "subSetM"), LookUpLong(md, "MS"));
    }
    // TODO Remove Part of parameter mapping now
    // withFirstOf(ValueSetter{g, "generatingProcessIdentifier"}, LookUpLong(md,
    // "generatingProcessIdentifier"));

    return ret;
}

void applyOverwrites(GribEncoder& g, const message::Metadata& md) {
    if (md.has("encoderOverwrites")) {
        // TODO Refactor with visitor
        auto overwrites = md.getSubConfiguration("encoderOverwrites");
        for (const auto& k : overwrites.keys()) {
            // TODO handle type... however eccodes should support string as well. For
            // some representations the string and integer representation in eccodes
            // differ significantly and my produce wrong results
            g.setValue(k, overwrites.getString(k));
        }
    }
}

// int GribEncoder::getBitsPerValue(int paramid, const std::string& levtype,
// double min, double max) {
//     return encodeBitsPerValue_.getBitsPerValue(paramid, levtype, min, max);
// }

void setEncodingSpecificFields(GribEncoder& g, const message::Message& msg) {
    const auto& md = msg.metadata();
    // TODO globalSize is expected to be set in md directly. nmuberOf* should be
    // readonly anyway... test removal..
    auto gls = lookUpLong(md, "globalSize");
    withFirstOf(ValueSetter{g, "numberOfDataPoints"}, gls);
    withFirstOf(ValueSetter{g, "numberOfValues"}, gls);

    withFirstOf(ValueSetter{g, "missingValue"}, LookUpDouble(md, "missingValue"));
    withFirstOf(ValueSetter{g, "bitmapPresent"}, LookUpBool(md, "bitmapPresent"));
    withFirstOf(ValueSetter{g, "bitsPerValue"}, LookUpLong(md, "bitsPerValue"));
}

void setDateAndStatisticalFields(GribEncoder& g, const eckit::Configuration& md,
                                 const QueriedMarsKeys& queriedMarsFields) {
    auto date = firstOf(
        LookUpLong(md, (queriedMarsFields.type && (*queriedMarsFields.type == "fc")) ? "startTime" : "currentDate"),
        LookUpLong(md, "startDate"));
    if (date) {
        g.setValue("year", *date / 10000);
        g.setValue("month", (*date % 10000) / 100);
        g.setValue("day", *date % 100);
    }
    auto time = firstOf(
        LookUpLong(md, (queriedMarsFields.type && (*queriedMarsFields.type == "fc")) ? "startTime" : "currentDate"),
        LookUpLong(md, "startTime"));
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
        if (*queriedMarsFields.type == "fc" && *operation == "instant") {
            // stepInHours has been set by statistics action
            withFirstOf(ValueSetter{g, "step"}, LookUpLong(md, "stepInHours"));
        }
        else {
            // stepRangeInHours has been set by statistics action
            withFirstOf(ValueSetter{g, "stepRange"}, LookUpLong(md, "stepRangeInHours"));
        }

        eckit::Optional<long> curDate;
        if (*operation != "instant" && (curDate = firstOf(LookUpLong(md, "currentDate")))) {
            g.setValue("typeOfStatisticalProcessing", type_of_statistical_processing.at(*operation));

            g.setValue("yearOfEndOfOverallTimeInterval", *curDate / 10000);
            g.setValue("monthOfEndOfOverallTimeInterval", (*curDate % 10000) / 100);
            g.setValue("dayOfEndOfOverallTimeInterval", *curDate % 100);
            g.setValue("lengthOfTimeRange", md.getLong("timeSpanInHours"));
            g.setValue("indicatorOfUnitForTimeIncrement", 13l);  // always seconds
            g.setValue("timeIncrement", md.getLong("timeStep"));
        }
    }
}

void GribEncoder::setFieldMetadata(const message::Message& msg) {
    if (isOcean(msg.metadata())) {
        setOceanMetadata(msg);
    }
    else {
        auto queriedMarsFields = setMarsKeys(*this, msg.metadata());
        applyOverwrites(*this, msg.metadata());
        setEncodingSpecificFields(*this, msg);
        setDateAndStatisticalFields(*this, msg.metadata(), queriedMarsFields);
    }
}

void GribEncoder::setOceanMetadata(const message::Message& msg) {
    auto runConfig = config_.getSubConfiguration("run");
    const auto& metadata = msg.metadata();

    auto queriedMarsFields = setMarsKeys(*this, runConfig);
    if (queriedMarsFields.type) {
        setValue("typeOfGeneratingProcess", type_of_generating_process.at(*queriedMarsFields.type));
    }
    applyOverwrites(*this, msg.metadata());
    setDateAndStatisticalFields(*this, runConfig, queriedMarsFields);
    setEncodingSpecificFields(*this, msg);

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

void GribEncoder::setOceanCoordMetadata(const message::Metadata& metadata) {
    setOceanCoordMetadata(metadata, config_.getSubConfiguration("run"));
}
void GribEncoder::setOceanCoordMetadata(const message::Metadata& md, const eckit::Configuration& runConfig) {
    // Set run-specific md
    setMarsKeys(*this, runConfig);

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

// TODO refactor - maybe throw exception instead of letting eccodes panic and
// disrupt exception system
void codesCheckRelaxed(int ret, const std::string& name) {
    if (ret == CODES_READ_ONLY) {
        // If value is read only, do not panic...
        eckit::Log::info() << "Multio GribEncoder: Ignoring readonly field " << name << std::endl;
        return;
    }
    if (ret != 0) {
        eckit::Log::info() << "Multio GribEncoder: CODES return value != NULL for "
                              "operation on field: "
                           << name << std::endl;
    }
    CODES_CHECK(ret, NULL);
}

void GribEncoder::setValue(const std::string& key, long value) {
    LOG_DEBUG_LIB(LibMultio) << "*** Setting value " << value << " for key " << key << std::endl;
    codesCheckRelaxed(codes_set_long(raw(), key.c_str(), value), key);
}

void GribEncoder::setValue(const std::string& key, double value) {
    LOG_DEBUG_LIB(LibMultio) << "*** Setting value " << value << " for key " << key << std::endl;
    codesCheckRelaxed(codes_set_double(raw(), key.c_str(), value), key);
}

void GribEncoder::setValue(const std::string& key, const std::string& value) {
    LOG_DEBUG_LIB(LibMultio) << "*** Setting value " << value << " for key " << key << std::endl;
    size_t sz = value.size();
    codesCheckRelaxed(codes_set_string(raw(), key.c_str(), value.c_str(), &sz), key);
}

void GribEncoder::setValue(const std::string& key, const unsigned char* value) {
    std::ostringstream oss;
    for (int i = 0; i < DIGEST_LENGTH; ++i) {
        oss << ((i == 0) ? "" : "-") << std::hex << std::setfill('0') << std::setw(2) << static_cast<short>(value[i]);
    }
    LOG_DEBUG_LIB(LibMultio) << "*** Setting value " << oss.str() << " for key " << key << std::endl;
    size_t sz = DIGEST_LENGTH;
    codesCheckRelaxed(codes_set_bytes(raw(), key.c_str(), value, &sz), key);
}

void GribEncoder::setValue(const std::string& key, bool value) {
    LOG_DEBUG_LIB(LibMultio) << "*** Setting value " << value << "(" << static_cast<long>(value) << ") for key " << key
                             << std::endl;
    codesCheckRelaxed(codes_set_long(raw(), key.c_str(), static_cast<long>(value)), key);
}

message::Message GribEncoder::encodeOceanLatitudes(const std::string& subtype) {
    auto msg = grids().at(subtype)->latitudes();

    setOceanCoordMetadata(msg.metadata());

    return dispatchPrecisionTag(msg.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        return setFieldValues<Precision>(std::move(msg));
    });
}

message::Message GribEncoder::encodeOceanLongitudes(const std::string& subtype) {
    auto msg = grids().at(subtype)->longitudes();

    setOceanCoordMetadata(msg.metadata());

    return dispatchPrecisionTag(msg.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        return setFieldValues<Precision>(std::move(msg));
    });
}

message::Message GribEncoder::encodeField(const message::Message& msg) {
    setFieldMetadata(msg);
    return dispatchPrecisionTag(msg.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        return setFieldValues<Precision>(std::move(msg));
    });
}

message::Message GribEncoder::encodeField(const message::Message& msg, const double* data, size_t sz) {
    setFieldMetadata(msg);
    return setFieldValues(data, sz);
}

message::Message GribEncoder::encodeField(const message::Message& msg, const float* data, size_t sz) {
    setFieldMetadata(msg);
    return setFieldValues(data, sz);
}

void GribEncoder::setDataValues(const float* data, size_t count) {

    std::vector<double> dvalues(count, 0.0);
    auto values = reinterpret_cast<const float*>(data);
    for (int i = 0; i < count; ++i) {
        dvalues[i] = double(values[i]);
    }

    this->setDataValues(dvalues.data(), count);

    return;
}


template <typename T>
message::Message GribEncoder::setFieldValues(const message::Message& msg) {

    auto beg = reinterpret_cast<const T*>(msg.payload().data());

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

message::Message GribEncoder::setFieldValues(const float* values, size_t count) {

    std::vector<double> dvalues(count, 0.0);
    for (int i = 0; i < count; ++i) {
        dvalues[i] = double(values[i]);
    }

    this->setDataValues(dvalues.data(), count);

    eckit::Buffer buf{this->length()};
    this->write(buf);

    return Message{Message::Header{Message::Tag::Grib, Peer{}, Peer{}}, std::move(buf)};
}


void GribEncoder::print(std::ostream& os) const {
    os << "GribEncoder(config=" << config_ << ")";
};

}  // namespace action
}  // namespace multio
