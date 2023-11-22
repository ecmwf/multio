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

#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"
#include "eckit/utils/MD5.h"
#include "eckit/utils/Translator.h"

#include "multio/LibMultio.h"
#include "multio/util/Metadata.h"


#include "multio/util/PrecisionTag.h"

#define DIGEST_LENGTH MD5_DIGEST_LENGTH

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
    T&& operator()(T&& t) {
        g_.setValue(key_, t);
        return std::forward<T>(t);
    }
};

eckit::Optional<ValueSetter> valueSetter(GribEncoder& g, const std::string& key) {
    if (g.hasKey(key.c_str())) {
        return eckit::Optional<ValueSetter>(ValueSetter{g, key});
    }
    else {
        return eckit::Optional<ValueSetter>();
    }
}

}  // namespace

GribEncoder::GribEncoder(codes_handle* handle, const eckit::LocalConfiguration& config) :
    template_{handle}, encoder_{nullptr}, config_{config} /*, encodeBitsPerValue_(config)*/ {}

struct QueriedMarsKeys {
    eckit::Optional<std::string> type{};
    eckit::Optional<long> paramId{};
};

QueriedMarsKeys setMarsKeys(GribEncoder& g, const eckit::Configuration& md) {
    QueriedMarsKeys ret;
    // TODO we should be able to determine the type in the metadata and preserve
    // it Domain usually is always readonly withFirstOf(valueSetter(g, "domain"),
    // LookUpString(md, "domain"), LookUpString(md, "globalDomain"));
    withFirstOf(valueSetter(g, "levtype"), LookUpString(md, "levtype"), LookUpString(md, "indicatorOfTypeOfLevel"));
    withFirstOf(valueSetter(g, "level"), LookUpLong(md, "level"), LookUpLong(md, "levelist"));
    withFirstOf(valueSetter(g, "date"), LookUpLong(md, "date"), LookUpLong(md, "dataDate"));
    withFirstOf(valueSetter(g, "time"), LookUpLong(md, "time"), LookUpLong(md, "dataTime"));
    withFirstOf(valueSetter(g, "step"), LookUpLong(md, "step"), LookUpLong(md, "startStep"));

    eckit::Optional<std::string> paramId{firstOf(
        LookUpString(md, "paramId"), LookUpString(md, "param"))};  // param might be a string, separated by . for GRIB1.
                                                                   // String to long convertion should get it right


    if (paramId) {
        g.setValue("paramId", eckit::Translator<std::string, long>{}(*paramId));
    }

    withFirstOf(valueSetter(g, "class"), LookUpString(md, "class"), LookUpString(md, "marsClass"));
    withFirstOf(valueSetter(g, "stream"), LookUpString(md, "stream"), LookUpString(md, "marsStream"));
    withFirstOf(valueSetter(g, "generatingProcessIdentifier"), LookUpString(md, "generatingProcessIdentifier"));
    withFirstOf(valueSetter(g, "expver"), LookUpString(md, "expver"), LookUpString(md, "experimentVersionNumber"));
    withFirstOf(valueSetter(g, "number"), LookUpLong(md, "ensemble-member"));
    withFirstOf(valueSetter(g, "numberOfForecastsInEnsemble"), LookUpLong(md, "ensemble-size"));

    auto dateOfAnalysis = firstOf(LookUpLong(md, "date-of-analysis"));
    if (dateOfAnalysis) {
        withFirstOf(valueSetter(g, "yearOfAnalysis"), eckit::Optional<long>{*dateOfAnalysis / 10000});
        withFirstOf(valueSetter(g, "monthOfAnalysis"), eckit::Optional<long>{(*dateOfAnalysis % 10000) / 100});
        withFirstOf(valueSetter(g, "dayOfAnalysis"), eckit::Optional<long>{*dateOfAnalysis % 100});
    }

    auto timeOfAnalysis = firstOf(LookUpLong(md, "time-of-analysis"));
    if (timeOfAnalysis) {
        withFirstOf(valueSetter(g, "hourOfAnalysis"), eckit::Optional<long>{*timeOfAnalysis / 10000});
        withFirstOf(valueSetter(g, "minuteOfAnalysis"), eckit::Optional<long>{(*timeOfAnalysis % 10000) / 100});
    }

    ret.type = firstOf(LookUpString(md, "type"), LookUpString(md, "marsType"));
    if (ret.type) {
        g.setValue("type", *ret.type);
    }

    // Additional parameters passed through for spherical harmonics
    if (md.has("gridType")) {
        auto hasRegularLLInterpData = [&]() {
            return md.has("Ni") && md.has("Nj") && md.has("north") && md.has("south") && md.has("west")
                && md.has("east") && md.has("west_east_increment") && md.has("south_north_increment");
        };
        if (md.getString("gridType") == "sh") {
            withFirstOf(valueSetter(g, "complexPacking"), LookUpLong(md, "complexPacking"));
            withFirstOf(valueSetter(g, "pentagonalResolutionParameterJ"),
                        LookUpLong(md, "pentagonalResolutionParameterJ"), LookUpLong(md, "J"));
            withFirstOf(valueSetter(g, "pentagonalResolutionParameterK"),
                        LookUpLong(md, "pentagonalResolutionParameterK"), LookUpLong(md, "K"));
            withFirstOf(valueSetter(g, "pentagonalResolutionParameterM"),
                        LookUpLong(md, "pentagonalResolutionParameterM"), LookUpLong(md, "M"));
            // withFirstOf(ValueSetter{g, "unpackedSubsetPrecision"}, LookUpLong(md,
            // "unpackedSubsetPrecision"));
            withFirstOf(valueSetter(g, "subSetJ"), LookUpLong(md, "subSetJ"), LookUpLong(md, "JS"));
            withFirstOf(valueSetter(g, "subSetK"), LookUpLong(md, "subSetK"), LookUpLong(md, "KS"));
            withFirstOf(valueSetter(g, "subSetM"), LookUpLong(md, "subSetM"), LookUpLong(md, "MS"));
        }
        else if (md.getString("gridType") == "regular_ll" && hasRegularLLInterpData()) {
            long scale = 0;
            if (md.getString("gribEdition") == "1") {
                scale = 1000;
            }
            else if (md.getString("gribEdition") == "2") {
                scale = 1000000;
            }
            g.setValue("Ni", md.getLong("Ni"));
            g.setValue("Nj", md.getLong("Nj"));
            double east = md.getDouble("east") - md.getDouble("west_east_increment");
            g.setValue("latitudeOfFirstGridPoint", scale * md.getDouble("north"));
            g.setValue("longitudeOfFirstGridPoint", scale * md.getDouble("west"));
            g.setValue("latitudeOfLastGridPoint", scale * md.getDouble("south"));
            g.setValue("longitudeOfLastGridPoint", scale * east);
            g.setValue("iDirectionIncrement", scale * md.getDouble("west_east_increment"));
            g.setValue("jDirectionIncrement", scale * md.getDouble("south_north_increment"));
        }
    }
    // TODO Remove Part of parameter mapping now
    // withFirstOf(valueSetter(g, "generatingProcessIdentifier"), LookUpLong(md,
    // "generatingProcessIdentifier"));

    return ret;
}

void applyOverwrites(GribEncoder& g, const message::Metadata& md) {
    if (md.has("encoder-overwrites")) {
        // TODO Refactor with visitor
        auto overwrites = md.getSubConfiguration("encoder-overwrites");
        for (const auto& k : overwrites.keys()) {
            // TODO handle type... however eccodes should support string as well. For
            // some representations the string and integer representation in eccodes
            // differ significantly and my produce wrong results
            if (g.hasKey(k.c_str())) {
                g.setValue(k, overwrites.getString(k));
            }
        }
    }
}

// int GribEncoder::getBitsPerValue(int paramid, const std::string& levtype,
// double min, double max) {
//     return encodeBitsPerValue_.getBitsPerValue(paramid, levtype, min, max);
// }

void setEncodingSpecificFields(GribEncoder& g, const eckit::Configuration& md) {
    // TODO globalSize is expected to be set in md directly. nmuberOf* should be
    // readonly anyway... test removal..
    auto gls = lookUpLong(md, "globalSize");
    withFirstOf(valueSetter(g, "numberOfDataPoints"), gls);
    withFirstOf(valueSetter(g, "numberOfValues"), gls);

    withFirstOf(valueSetter(g, "missingValue"), LookUpDouble(md, "missingValue"));
    withFirstOf(valueSetter(g, "bitmapPresent"), LookUpBool(md, "bitmapPresent"));
    withFirstOf(valueSetter(g, "bitsPerValue"), LookUpLong(md, "bitsPerValue"));
}

namespace {
eckit::Optional<long> marsDate(const message::Metadata& md, const QueriedMarsKeys& mKeys) {
    if (not mKeys.type) {
        return eckit::Optional<long>{};
    }

    // List of forecast-type data
    if ((*mKeys.type == "fc") || (*mKeys.type == "pf")) {
        return firstOf(LookUpLong(md, "startDate"));
    }

    // List time-processed analysis data
    if (*mKeys.type == "tpa") {
        return firstOf(LookUpLong(md, "previousDate"));
    }

    // Analysis data
    return firstOf(LookUpLong(md, "currentDate"));
}

eckit::Optional<long> marsTime(const message::Metadata& md, const QueriedMarsKeys& mKeys) {
    if (not mKeys.type) {
        return eckit::Optional<long>{};
    }

    // List of forecast-type data
    if ((*mKeys.type == "fc") || (*mKeys.type == "pf")) {
        return firstOf(LookUpLong(md, "startTime"));
    }

    // List time-processed analysis data
    if (*mKeys.type == "tpa") {
        return firstOf(LookUpLong(md, "previousTime"));
    }

    // Analysis data
    return firstOf(LookUpLong(md, "currentTime"));
}

std::string marsStepRange(const message::Metadata& md, const QueriedMarsKeys& mKeys) {

    auto stepInHours = firstOf(LookUpLong(md, "stepInHours"));
    auto timeSpanInHours = firstOf(LookUpLong(md, "timeSpanInHours"));
    if (not(stepInHours && timeSpanInHours)) {
        throw eckit::SeriousBug("Not enough information to encode step range");
    }

    // List of forecast-type data
    if ((*mKeys.type == "fc") || (*mKeys.type == "pf")) {
        auto prevStep = std::max(*stepInHours - *timeSpanInHours, 0L);
        return std::to_string(prevStep) + "-" + std::to_string(*stepInHours);
    }

    // Time-processed analysis
    return std::to_string(0) + "-" + std::to_string(*timeSpanInHours);
}

}  // namespace

void setDateAndStatisticalFields(GribEncoder& g, const eckit::Configuration& md,
                                 const QueriedMarsKeys& queriedMarsFields) {

    auto date = marsDate(md, queriedMarsFields);
    if (date) {
        withFirstOf(valueSetter(g, "year"), eckit::Optional<long>{*date / 10000});
        withFirstOf(valueSetter(g, "month"), eckit::Optional<long>{(*date % 10000) / 100});
        withFirstOf(valueSetter(g, "day"), eckit::Optional<long>{*date % 100});
    }

    auto time = marsTime(md, queriedMarsFields);
    if (time) {
        withFirstOf(valueSetter(g, "hour"), eckit::Optional<long>{*time / 10000});
        withFirstOf(valueSetter(g, "minute"), eckit::Optional<long>{(*time % 10000) / 100});
        withFirstOf(valueSetter(g, "second"), eckit::Optional<long>{*time % 100});
    }

    auto operation = lookUpString(md, "operation");
    if (operation) {
        if (*queriedMarsFields.type == "fc" && *operation == "instant") {
            // stepInHours has been set by statistics action
            withFirstOf(valueSetter(g, "step"), LookUpLong(md, "stepInHours"));
        }
        else {
            // Setting directly as it is computed value not read from the metadata
            g.setValue("stepRange", marsStepRange(md, queriedMarsFields));
            //withFirstOf(valueSetter(g, "stepRange"), LookUpString(md, "stepRangeInHours"));
        }

        eckit::Optional<long> curDate;
        if (*operation != "instant" && (curDate = firstOf(LookUpLong(md, "currentDate")))) {
            withFirstOf(valueSetter(g, "typeOfStatisticalProcessing"),
                        eckit::Optional<long>{type_of_statistical_processing.at(*operation)});

            withFirstOf(valueSetter(g, "yearOfEndOfOverallTimeInterval"), eckit::Optional<long>{*curDate / 10000});
            withFirstOf(valueSetter(g, "monthOfEndOfOverallTimeInterval"),
                        eckit::Optional<long>{(*curDate % 10000) / 100});
            withFirstOf(valueSetter(g, "dayOfEndOfOverallTimeInterval"), eckit::Optional<long>{*curDate % 100});

            withFirstOf(valueSetter(g, "lengthOfTimeRange"), LookUpLong(md, "timeSpanInHours"));
            withFirstOf(valueSetter(g, "indicatorOfUnitForTimeIncrement"),
                        eckit::Optional<long>{13l});  // always seconds
            withFirstOf(valueSetter(g, "timeIncrement"), LookUpLong(md, "timeStep"));
        }
    }
}

void GribEncoder::setFieldMetadata(const message::Message& msg) {
    if (isOcean(msg.metadata())) {
        setOceanMetadata(msg);
    }
    else {
        const auto& metadata = msg.metadata();
        auto queriedMarsFields = setMarsKeys(*this, metadata);
        applyOverwrites(*this, metadata);
        setEncodingSpecificFields(*this, metadata);
        setDateAndStatisticalFields(*this, metadata, queriedMarsFields);
    }
}

void GribEncoder::setOceanMetadata(const message::Message& msg) {
    auto runConfig = config_.getSubConfiguration("run");
    const auto& metadata = msg.metadata();

    auto queriedMarsFields = setMarsKeys(*this, runConfig);
    if (queriedMarsFields.type) {
        setValue("typeOfGeneratingProcess", type_of_generating_process.at(*queriedMarsFields.type));
    }
    applyOverwrites(*this, metadata);
    setDateAndStatisticalFields(*this, metadata, queriedMarsFields);
    setEncodingSpecificFields(*this, metadata);

    // Setting parameter ID
    if (metadata.getLong("param") / 1000 == 212) {
        // HACK! Support experimental averages.
        setValue("paramId", metadata.getLong("param") + 4000);
    } else {
        setValue("paramId", metadata.getLong("param") + ops_to_code.at(metadata.getString("operation")));
    }
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

    const auto& gridUID = metadata.getString("uuidOfHGrid");
    setValue("uuidOfHGrid", gridUID);
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

    const auto& gridUID = md.getString("uuidOfHGrid");
    setValue("uuidOfHGrid", gridUID);

    // Set encoding for missing value support
    setValue("bitmapPresent", false);
    setValue("bitsPerValue", md.getLong("bitsPerValue"));
}


void GribEncoder::initEncoder() {
    encoder_.reset(template_.duplicate());
    return;
};

bool GribEncoder::hasKey(const char* key) {
    return encoder_->hasKey(key);
};

message::Message GribEncoder::encodeOceanCoordinates(message::Message&& msg) {
    initEncoder();

    setOceanCoordMetadata(msg.metadata());

    return dispatchPrecisionTag(msg.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        return setFieldValues<Precision>(std::move(msg));
    });
}

message::Message GribEncoder::encodeField(const message::Message& msg) {
    initEncoder();
    setFieldMetadata(msg);
    return dispatchPrecisionTag(msg.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        return setFieldValues<Precision>(std::move(msg));
    });
}

message::Message GribEncoder::encodeField(const message::Message& msg, const double* data, size_t sz) {
    initEncoder();
    setFieldMetadata(msg);
    return setFieldValues(data, sz);
}

message::Message GribEncoder::encodeField(const message::Message& msg, const float* data, size_t sz) {
    initEncoder();
    setFieldMetadata(msg);
    return setFieldValues(data, sz);
}


template <typename T>
message::Message GribEncoder::setFieldValues(const message::Message& msg) {
    auto beg = reinterpret_cast<const T*>(msg.payload().data());

    this->setDataValues(beg, msg.globalSize());

    eckit::Buffer buf{this->encoder_->length()};
    encoder_->write(buf);

    return Message{Message::Header{Message::Tag::Grib, Peer{msg.source().group()}, Peer{msg.destination()}},
                   std::move(buf)};
}


message::Message GribEncoder::setFieldValues(const double* values, size_t count) {
    encoder_->setDataValues(values, count);

    eckit::Buffer buf{this->encoder_->length()};
    encoder_->write(buf);

    return Message{Message::Header{Message::Tag::Grib, Peer{}, Peer{}}, std::move(buf)};
}

message::Message GribEncoder::setFieldValues(const float* values, size_t count) {
    std::vector<double> dvalues(count, 0.0);
    for (int i = 0; i < count; ++i) {
        dvalues[i] = double(values[i]);
    }

    encoder_->setDataValues(dvalues.data(), count);

    eckit::Buffer buf{this->encoder_->length()};
    encoder_->write(buf);

    return Message{Message::Header{Message::Tag::Grib, Peer{}, Peer{}}, std::move(buf)};
}


void GribEncoder::print(std::ostream& os) const {
    os << "GribEncoder(config=" << config_ << ")";
};

}  // namespace action
}  // namespace multio
