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
#include <functional>
#include <iomanip>
#include <iostream>
#include <unordered_map>

#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"
#include "eckit/types/DateTime.h"
#include "eckit/utils/MD5.h"
#include "eckit/utils/StringTools.h"
#include "eckit/utils/Translator.h"
#include "eckit/value/Value.h"


#include "multio/LibMultio.h"
#include "multio/util/DateTime.h"
#include "multio/util/Metadata.h"


#include "multio/util/PrecisionTag.h"

#define DIGEST_LENGTH MD5_DIGEST_LENGTH

namespace multio::action {

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


// // https://codes.ecmwf.int/grib/format/grib2/ctables/4/4/
std::int64_t timeUnitCodes(util::TimeUnit u) {
    switch (u) {
        case util::TimeUnit::Year:
            return 4;
        case util::TimeUnit::Month:
            return 3;
        case util::TimeUnit::Day:
            return 2;
        case util::TimeUnit::Hour:
            return 1;
        case util::TimeUnit::Minute:
            return 0;
        case util::TimeUnit::Second:
            return 13;
        default:
            std::ostringstream oss;
            oss << "timeUnitCodes: Unexpcted TimeUnit " << util::timeUnitToChar(u);
            throw eckit::SeriousBug(std::string(oss.str()), Here());
    }
}


std::tuple<std::int64_t, std::int64_t> getReferenceDateTime(const std::string& timeRef,
                                                            const eckit::Configuration& in) {
    static std::unordered_map<std::string, std::tuple<std::string, std::string>> REF_TO_DATETIME_KEYS{
        {"start", {"startDate", "startTime"}},
        {"previous", {"previousDate", "previousTime"}},
        {"current", {"currentDate", "currentTime"}},
    };

    auto search = REF_TO_DATETIME_KEYS.find(timeRef);

    return std::make_tuple(in.getLong(std::get<0>(search->second)), in.getLong(std::get<1>(search->second)));
}


void tryMapStepToTimeAndCheckTime(eckit::LocalConfiguration& in) {

    bool hasStartDateTime = (in.has("startDate") && in.has("startTime"));
    bool hasDataDateTime = (in.has("dataDate") && in.has("dataTime"));
    bool hasDateTime = (in.has("date") && in.has("time"));

    // std::cout << "tryMapStepToTimeAndCheckTime..." << std::endl;
    if (hasStartDateTime || hasDateTime || hasDataDateTime) {
        util::DateInts startDate;
        util::TimeInts startTime;

        if (hasStartDateTime) {
            startDate = util::toDateInts(in.getLong("startDate"));
            startTime = util::toTimeInts(in.getLong("startTime"));
        }
        else if (hasDataDateTime) {
            startDate = util::toDateInts(in.getLong("dataDate"));
            startTime = util::toTimeInts(in.getLong("dataTime"));
        }
        else if (hasDateTime) {
            startDate = util::toDateInts(in.getLong("date"));
            startTime = util::toTimeInts(in.getLong("time") * 100);

            in.set("startDate", in.getLong("date"));
            in.set("startTime", in.getLong("time") * 100);
        }

        // std::cout << "startDate: " << startDate.year << " " << startDate.month << " " << startDate.day << std::endl;
        // std::cout << "startTime: " << startTime.hour << " " << startTime.minute << " " << startTime.second  <<
        // std::endl;

        eckit::DateTime startDateTime(eckit::Date(startDate.year, startDate.month, startDate.day),
                                      eckit::Time(startTime.hour, startTime.minute, startTime.second));

        if (in.has("step") && (!in.has("currentDate") || !in.has("currentTime"))) {
            std::int64_t step = in.getLong("step");

            // IFS default step unit is hours
            auto currentDateTime = startDateTime + (step * 3600);

            in.set("currentDate", currentDateTime.date().yyyymmdd());
            in.set("currentTime", currentDateTime.time().hhmmss());
        }
        if ((in.has("stepRange") || (in.has("startStep") && in.has("endStep")))
            && (!in.has("currentDate") || !in.has("currentTime") || !in.has("previousDate")
                || !in.has("previousTime"))) {

            std::int64_t stepStart;
            std::int64_t stepEnd;

            if (in.has("stepRange")) {
                std::string stepRange = in.getString("stepRange");
                auto split = stepRange.find("-");

                if (split == std::string::npos) {
                    std::ostringstream oss;
                    oss << "tryMapStepToTime: field \"stepRange\" is expected to contain a \"-\": " << stepRange;
                    throw eckit::SeriousBug(oss.str(), Here());
                }

                stepStart = eckit::translate<std::int64_t>(stepRange.substr(0, split));
                stepEnd = eckit::translate<std::int64_t>(stepRange.substr(split + 1));
            }
            else {
                stepStart = in.getLong("startStep");
                stepEnd = in.getLong("endStep");
            }

            // IFS default step unit is hours
            auto previousDateTime = startDateTime + (stepStart * 3600);

            in.set("previousDate", previousDateTime.date().yyyymmdd());
            in.set("previousTime", previousDateTime.time().hhmmss());

            // IFS default step unit is hours
            auto currentDateTime = startDateTime + (stepEnd * 3600);

            in.set("currentDate", currentDateTime.date().yyyymmdd());
            in.set("currentTime", currentDateTime.time().hhmmss());
        }
    }


    // Compute back from currentDate/Time + endStep/startStep

    if (!in.has("currentDate") || !in.has("currentTime")) {
        throw eckit::UserError(
            "tryMapStepToTime: Grib encoding requires at least date time fields {\"currentTime\" and \"currentDate\"}, "
            "or {\"startTime\" and \"startDate\" and {\"step\" or {\"stepRange\", or \"startStep\" and \"endStep\"}}}",
            Here());
    }
}

struct ValueSetter {
    GribEncoder& g_;
    std::string key_;

    template <typename T>
    T&& operator()(T&& t) {
        g_.setValue(key_, t);
        return std::forward<T>(t);
    }
};

std::optional<ValueSetter> valueSetter(GribEncoder& g, const std::string& key) {
    if (g.hasKey(key.c_str())) {
        return std::optional<ValueSetter>(ValueSetter{g, key});
    }
    else {
        return std::optional<ValueSetter>();
    }
}

}  // namespace

GribEncoder::GribEncoder(codes_handle* handle, const eckit::LocalConfiguration& config) :
    template_{handle}, encoder_{nullptr}, config_{config} /*, encodeBitsPerValue_(config)*/ {}

struct QueriedMarsKeys {
    std::optional<std::string> type{};
    std::optional<long> paramId{};
};

QueriedMarsKeys setMarsKeys(GribEncoder& g, const eckit::Configuration& md) {
    QueriedMarsKeys ret;

    // TODO we should be able to determine the type in the metadata and preserve
    // it Domain usually is always readonly withFirstOf(valueSetter(g, "domain"),
    // LookUpString(md, "domain"), LookUpString(md, "globalDomain"));
    std::string gridType;
    const auto hasGridType = md.get("gridType", gridType);

    const auto gribEdition = md.getString("gribEdition", "2");

    std::string typeOfLevel;
    const auto hasTypeOfLevel = md.get("typeOfLevel", typeOfLevel);
    if (hasTypeOfLevel) {
        g.setValue("typeOfLevel", typeOfLevel);
    }

    // param might be a string, separated by . for GRIB1.
    std::optional<std::string> paramId{firstOf(LookUpString(md, "paramId"), LookUpString(md, "param"))};

    // String to long convertion should get it right
    if (paramId) {
        g.setValue("paramId", eckit::Translator<std::string, long>{}(*paramId));
    }

    if (gribEdition == "2") {
        withFirstOf(valueSetter(g, "subCentre"), LookUpString(md, "subCentre"));
        withFirstOf(valueSetter(g, "productionStatusOfProcessedData"),
                    LookUpLong(md, "productionStatusOfProcessedData"));

        if (md.has("dataset")) {
            withFirstOf(valueSetter(g, "tablesVersion"), LookUpLong(md, "tablesVersion"));
            withFirstOf(valueSetter(g, "setLocalDefinition"), LookUpLong(md, "setLocalDefinition"));
            withFirstOf(valueSetter(g, "grib2LocalSectionNumber"), LookUpLong(md, "grib2LocalSectionNumber"));

            const auto dataset = md.getString("dataset");
            g.setValue("dataset", dataset);

            if (dataset == "climate-dt") {
                withFirstOf(valueSetter(g, "activity"), LookUpString(md, "activity"));
                withFirstOf(valueSetter(g, "experiment"), LookUpString(md, "experiment"));
                withFirstOf(valueSetter(g, "generation"), LookUpString(md, "generation"));
                withFirstOf(valueSetter(g, "model"), LookUpString(md, "model"));
                withFirstOf(valueSetter(g, "realization"), LookUpString(md, "realization"));
            }
        }
    }

    withFirstOf(valueSetter(g, "class"), LookUpString(md, "class"), LookUpString(md, "marsClass"));
    withFirstOf(valueSetter(g, "stream"), LookUpString(md, "stream"), LookUpString(md, "marsStream"));

    withFirstOf(valueSetter(g, "generatingProcessIdentifier"), LookUpString(md, "generatingProcessIdentifier"));

    withFirstOf(valueSetter(g, "setPackingType"), LookUpString(md, "setPackingType"));

    withFirstOf(valueSetter(g, "expver"), LookUpString(md, "expver"), LookUpString(md, "experimentVersionNumber"));
    withFirstOf(valueSetter(g, "number"), LookUpLong(md, "ensemble-member"));
    withFirstOf(valueSetter(g, "numberOfForecastsInEnsemble"), LookUpLong(md, "ensemble-size"));
    withFirstOf(valueSetter(g, "methodNumber"), LookUpLong(md, "method-number"));
    withFirstOf(valueSetter(g, "systemNumber"), LookUpLong(md, "system-number"));

    ret.type = firstOf(LookUpString(md, "type"), LookUpString(md, "marsType"));
    if (ret.type) {
        g.setValue("type", *ret.type);
    }

    // Additional parameters passed through for spherical harmonics
    if (hasGridType) {
        auto hasRegularLLInterpData = [&]() {
            return md.has("Ni") && md.has("Nj") && md.has("north") && md.has("south") && md.has("west")
                && md.has("east") && md.has("west_east_increment") && md.has("south_north_increment");
        };
        if (gridType == "sh") {
            withFirstOf(valueSetter(g, "complexPacking"), LookUpLong(md, "complexPacking"));
            withFirstOf(valueSetter(g, "pentagonalResolutionParameterJ"),
                        LookUpLong(md, "pentagonalResolutionParameterJ"), LookUpLong(md, "J"));
            withFirstOf(valueSetter(g, "pentagonalResolutionParameterK"),
                        LookUpLong(md, "pentagonalResolutionParameterK"), LookUpLong(md, "K"));
            withFirstOf(valueSetter(g, "pentagonalResolutionParameterM"),
                        LookUpLong(md, "pentagonalResolutionParameterM"), LookUpLong(md, "M"));

            withFirstOf(valueSetter(g, "subSetJ"), LookUpLong(md, "subSetJ"), LookUpLong(md, "JS"));
            withFirstOf(valueSetter(g, "subSetK"), LookUpLong(md, "subSetK"), LookUpLong(md, "KS"));
            withFirstOf(valueSetter(g, "subSetM"), LookUpLong(md, "subSetM"), LookUpLong(md, "MS"));
        }
        else if (gridType == "regular_ll" && hasRegularLLInterpData()) {
            long scale = 0;
            if (gribEdition == "1") {
                scale = 1000;
            }
            else if (gribEdition == "2") {
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
        else if (eckit::StringTools::lower(gridType) == "healpix") {
            long Nside = md.getLong("Nside");
            g.setValue("Nside", Nside);
            double logp = 45.0;
            // Note: Pedro told to use always this to avoid problems with milli and micro degrees
            g.setValue("longitudeOfFirstGridPointInDegrees", logp);
            g.setValue("orderingConvention", md.getString("orderingConvention"));
        }
    }
    // TODO Remove Part of parameter mapping now
    // withFirstOf(valueSetter(g, "generatingProcessIdentifier"), LookUpLong(md,
    // "generatingProcessIdentifier"));

    return ret;
}

template <typename KVFunc>
void visitKeyValues(const eckit::Configuration& c, KVFunc&& func) {
    for (const auto& k : c.keys()) {
        auto val = c.getSubConfiguration(k).get();

        if (val.isBool()) {
            func(k, (bool)val);
        }
        else if (val.isNumber()) {
            func(k, (std::int64_t)val);
        }
        else if (val.isDouble()) {
            func(k, (double)val);
        }
        else if (val.isString()) {
            func(k, (std::string)val);
        }
        else {
            NOTIMP;
        }
    }
}

void applyOverwrites(GribEncoder& g, const message::Metadata& md) {
    if (md.has("encoder-overwrites")) {
        // TODO Refactor with visitor
        auto overwrites = md.getSubConfiguration("encoder-overwrites");
        visitKeyValues(overwrites, [&](const std::string& k, const auto& v) {
            if (g.hasKey(k.c_str())) {
                g.setValue(k, v);
            }
        });
    }
}

void setEncodingSpecificFields(GribEncoder& g, const eckit::Configuration& md) {
    // TODO globalSize is expected to be set in md directly. nmuberOf* should be
    // readonly anyway... test removal..

    withFirstOf(valueSetter(g, "missingValue"), LookUpDouble(md, "missingValue"));
    withFirstOf(valueSetter(g, "bitmapPresent"), LookUpBool(md, "bitmapPresent"));
    withFirstOf(valueSetter(g, "bitsPerValue"), LookUpLong(md, "bitsPerValue"));
}

void setDateAndStatisticalFields(GribEncoder& g, const eckit::LocalConfiguration& in,
                                 const QueriedMarsKeys& queriedMarsFields) {
    eckit::LocalConfiguration md = in;  // Copy to allow modification

    std::string gribEdition = md.getString("gribEdition", "2");

    auto operation = lookUpString(md, "operation");
    auto startStep = lookUpLong(md, "startStep");
    auto endStep = lookUpLong(md, "endStep");
    bool isTimeRange = (operation && (*operation != "instant"))
                    || (queriedMarsFields.type && *queriedMarsFields.type == "tpa")
                    || (endStep && startStep && (endStep != startStep));


    auto significanceOfReferenceTime = lookUpLong(md, "significanceOfReferenceTime");
    if (!significanceOfReferenceTime) {
        if (md.has("encoder-overwrites")) {
            auto overwrites = md.getSubConfiguration("encoder-overwrites");
            significanceOfReferenceTime = lookUpLong(overwrites, "significanceOfReferenceTime");
        }
    }
    if ((gribEdition == "2") && significanceOfReferenceTime) {
        g.setValue("significanceOfReferenceTime", *significanceOfReferenceTime);
    }

    tryMapStepToTimeAndCheckTime(md);

    std::string timeRef = std::invoke([&]() -> std::string {
        if (auto optTimeRef = lookUpString(md, "timeReference"); optTimeRef) {
            return *optTimeRef;
        }


        // TODO: this will not hold in the future - maybe the new category "processType" can be used to check if it's a
        // forecast
        // Handling of significanceOfReferenceTime is hacked in for now....
        bool isReferingToStart = false;
        if (queriedMarsFields.type) {
            if (*queriedMarsFields.type == "fc") {
                // If significanceOfReferenceTime is validityTime (2)
                // then forecastTime should be set to zero.
                if ((gribEdition == "2") && significanceOfReferenceTime && (*significanceOfReferenceTime == 2)) {
                    isReferingToStart = false;
                    g.setValue("stepUnits", timeUnitCodes(util::TimeUnit::Hour));
                    g.setValue("startStep", 0l);
                    if (gribEdition == "2") {
                        g.setValue("indicatorOfUnitOfTimeRange", timeUnitCodes(util::TimeUnit::Hour));
                        g.setValue("forecastTime", 0l);
                    }
                }
                else {
                    isReferingToStart = true;
                }
            }
            else if (queriedMarsFields.type == "pf") {
                isReferingToStart = true;
            }
        }
        return isReferingToStart ? "start" : (isTimeRange ? "previous" : "current");
    });

    auto refDateTimeTup = getReferenceDateTime(timeRef, md);
    auto refDateTime = util::wrapDateTime(
        {util::toDateInts(std::get<0>(refDateTimeTup)), util::toTimeInts(std::get<1>(refDateTimeTup))});
    g.setValue("year", refDateTime.date.year);
    g.setValue("month", refDateTime.date.month);
    g.setValue("day", refDateTime.date.day);

    g.setValue("hour", refDateTime.time.hour);
    g.setValue("minute", refDateTime.time.minute);
    g.setValue("second", refDateTime.time.second);

    auto currentDateTime = util::wrapDateTime(
        {util::toDateInts(md.getLong("currentDate")), util::toTimeInts(md.getLong("currentTime"))});

    if (!isTimeRange) {
        if (timeRef == std::string("start")) {
            // Compute diff to current time in some appropriate unit
            g.setValue("stepUnits", timeUnitCodes(util::TimeUnit::Hour));
            g.setValue("startStep", util::dateTimeDiffInSeconds(currentDateTime.date, currentDateTime.time,
                                                                refDateTime.date, refDateTime.time)
                                        / 3600);
        }
        else {
            g.setValue("stepUnits", timeUnitCodes(util::TimeUnit::Hour));
            g.setValue("startStep", 0l);
        }
    }
    else if (gribEdition == "2") {
        auto previousDateTime = util::wrapDateTime(
            {util::toDateInts(md.getLong("previousDate")), util::toTimeInts(md.getLong("previousTime"))});

        // Now just deal with GRIB2
        g.setValue("yearOfEndOfOverallTimeInterval", currentDateTime.date.year);
        g.setValue("monthOfEndOfOverallTimeInterval", currentDateTime.date.month);
        g.setValue("dayOfEndOfOverallTimeInterval", currentDateTime.date.day);
        g.setValue("hourOfEndOfOverallTimeInterval", currentDateTime.time.hour);
        g.setValue("minuteOfEndOfOverallTimeInterval", currentDateTime.time.minute);
        g.setValue("secondOfEndOfOverallTimeInterval", currentDateTime.time.second);

        g.setValue("indicatorOfUnitForTimeRange", timeUnitCodes(util::TimeUnit::Hour));
        g.setValue("lengthOfTimeRange", util::dateTimeDiffInSeconds(currentDateTime.date, currentDateTime.time,
                                                                    previousDateTime.date, previousDateTime.time)
                                            / 3600);

        if (timeRef == std::string("start")) {
            // Compute diff to current time in some appropriate unit
            g.setValue("stepUnits", timeUnitCodes(util::TimeUnit::Hour));
            g.setValue("startStep", util::dateTimeDiffInSeconds(previousDateTime.date, previousDateTime.time,
                                                                refDateTime.date, refDateTime.time)
                                        / 3600);

            // Set endStep to please MARS
            g.setValue("stepUnits", timeUnitCodes(util::TimeUnit::Hour));
            g.setValue("endStep", util::dateTimeDiffInSeconds(currentDateTime.date, currentDateTime.time,
                                                              refDateTime.date, refDateTime.time)
                                      / 3600);
        }
        else {
            // No forecast time is used
            g.setValue("stepUnits", timeUnitCodes(util::TimeUnit::Hour));
            g.setValue("startStep", 0l);
            if (gribEdition == "2") {
                g.setValue("indicatorOfUnitOfTimeRange", timeUnitCodes(util::TimeUnit::Hour));
                g.setValue("forecastTime", 0l);
            }


            // Set endStep to please MARS
            g.setValue("stepUnits", timeUnitCodes(util::TimeUnit::Hour));
            g.setValue("endStep", util::dateTimeDiffInSeconds(currentDateTime.date, currentDateTime.time,
                                                              previousDateTime.date, previousDateTime.time)
                                      / 3600);
        }

        if (operation) {
            static const std::map<const std::string, const std::int64_t> TYPE_OF_STATISTICAL_PROCESSING{
                {"average", 0}, {"accumulate", 1}, {"maximum", 2}, {"minimum", 3}, {"stddev", 6}};
            if (auto searchStat = TYPE_OF_STATISTICAL_PROCESSING.find(*operation);
                searchStat != TYPE_OF_STATISTICAL_PROCESSING.end()) {
                g.setValue("typeOfStatisticalProcessing", searchStat->second);
            }
            else {
                std::ostringstream oss;
                oss << "setDateAndStatisticalFields - Cannot map value \"" << *operation
                    << "\"for key \"operation\" (statistical output) to a valid grib2 type of statistical processing.";
                throw eckit::UserError(oss.str(), Here());
            }
        }


        // # CODE TABLE 4.11, Type of time intervals
        // 1 1  Successive times processed have same forecast time, start time of forecast is incremented
        // 2 2  Successive times processed have same start time of forecast, forecast time is incremented
        // 3 3  Successive times processed have start time of forecast incremented and forecast time decremented so that
        // valid time remains constant 4 4  Successive times processed have start time of forecast decremented and
        // forecast time incremented so that valid time remains constant 5 5  Floating subinterval of time between
        // forecast time and end of overall time interval
        // g.setValue("typeOfTimeIncrement", (timeRef == "start" ? 2 : 1));
        //
        // #### Work around ####
        // Eccodes has problems showing stepRange correctly for averaging fields with typeOfTimeIncrement == 1.
        // It seems that from this combination eccodes is infering a `stepKey` of avgd (daily average).
        // For daily average the stepRange is shown as 0 instead of 0-24 (desired). Hence with DGov we decided to put
        // 255 (MISSING) as typeOfTimeIncrement
        //
        // TO BE DISCUSSED - obviously there is some confusion about typeOfTimeIncrement=1. For analysis I read that it
        // should be set to 1. However eccodes thinks different and will not consider it as time range then... hence I
        // explicily set it to 255 now g.setValue(
        //     "typeOfTimeIncrement",
        //     (timeRef == "start" ? 2
        //                         : ((gribEdition == "2") && (significanceOfReferenceTime &&
        //                         (*significanceOfReferenceTime == 2)) ? 255 : 1)));
        g.setValue("typeOfTimeIncrement", (timeRef == "start" ? 2 : ((gribEdition == "2") ? 255 : 1)));

        if (const auto timeIncrement = lookUpLong(md, "timeIncrement"); timeIncrement) {
            if (*timeIncrement != 0) {
                withFirstOf(valueSetter(g, "indicatorOfUnitForTimeIncrement"),
                            LookUpLong(md, "indicatorOfUnitForTimeIncrement"));
                g.setValue("timeIncrement", *timeIncrement);
            }
            else {
                g.setValue("indicatorOfUnitForTimeIncrement", 255);
                g.setValue("timeIncrement", 0);
            }
        }
        else if (const auto sampleIntervalInSeconds = lookUpLong(md, "sampleIntervalInSeconds");
                 sampleIntervalInSeconds) {
            g.setValue("indicatorOfUnitForTimeIncrement", timeUnitCodes(util::TimeUnit::Second));
            g.setValue("timeIncrement", *sampleIntervalInSeconds);
        }
        else {
            g.setValue("indicatorOfUnitForTimeIncrement", timeUnitCodes(util::TimeUnit::Second));
            withFirstOf(valueSetter(g, "timeIncrement"),
                        LookUpLong(md, "timeStep"));  // Nemo is currently sending timeStep
        }
    }


    auto dateOfAnalysis = firstOf(LookUpLong(md, "date-of-analysis"));
    auto timeOfAnalysis = firstOf(LookUpLong(md, "time-of-analysis")).value_or(0);
    if (dateOfAnalysis) {
        auto analysisDateTime
            = util::wrapDateTime({util::toDateInts(*dateOfAnalysis), util::toTimeInts(timeOfAnalysis)});
        g.setValue("yearOfAnalysis", analysisDateTime.date.year);
        g.setValue("monthOfAnalysis", analysisDateTime.date.month);
        g.setValue("dayOfAnalysis", analysisDateTime.date.day);

        g.setValue("hourOfAnalysis", analysisDateTime.time.hour);
        g.setValue("minuteOfAnalysis", analysisDateTime.time.minute);
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

namespace {}

void GribEncoder::setOceanMetadata(const message::Message& msg) {
    // Copy metadata now to merge with run config
    auto metadata = msg.metadata();

    visitKeyValues(config_.getSubConfiguration("run"),
                   [&](const std::string& k, const auto& v) { metadata.set(k, v); });

    auto queriedMarsFields = setMarsKeys(*this, metadata);
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
    }
    else {
        setValue("paramId", metadata.getLong("param") + ops_to_code.at(metadata.getString("operation")));
    }
    const auto& typeOfLevel = metadata.getString("typeOfLevel");
    setValue("typeOfLevel", typeOfLevel);
    if (typeOfLevel == "oceanModelLayer") {
        auto level = metadata.getLong("level");
        ASSERT(level > 0);
        setValue("scaledValueOfFirstFixedSurface", level - 1);
        setValue("scaledValueOfSecondFixedSurface", level);
        setValue("scaleFactorOfFirstFixedSurface", 0l);
        setValue("scaleFactorOfSecondFixedSurface", 0l);
    }
    if (typeOfLevel == "oceanModel") {
        auto level = metadata.getLong("level");
        ASSERT(level > 0);
        setValue("scaledValueOfFirstFixedSurface", level);
        setValue("scaleFactorOfFirstFixedSurface", 0l);
    }

    std::string gridType;
    const auto hasGridType = metadata.get("gridType", gridType);
    if (hasGridType && gridType == "unstructured_grid") {
        std::string unstructuredGridType;
        const auto hasUnstructuredGridType = metadata.get("unstructuredGridType", unstructuredGridType);
        if (!hasUnstructuredGridType) {
            unstructuredGridType = config_.getString("unstructured-grid-type");
        }

        // Set ocean grid information
        setValue("unstructuredGridType", unstructuredGridType);

        if (metadata.has("unstructuredGridSubtype")) {
            setValue("unstructuredGridSubtype", metadata.getString("unstructuredGridSubtype"));
        }

        if (metadata.has("uuidOfHGrid")) {
            const auto& gridUID = metadata.getString("uuidOfHGrid");
            setValue("uuidOfHGrid", gridUID);
        }
        else {
            eckit::Log::warning() << "Ocean grid UUID not available during encoding!" << std::endl;
        }
    }
    else if (eckit::StringTools::lower(gridType) == "healpix") {
        long Nside = metadata.getLong("Nside");
        setValue("Nside", Nside);
        double logp = 45.0;
        // Note: Pedro told to use always this to avoid problems with milli and micro degrees
        setValue("longitudeOfFirstGridPointInDegrees", logp);
        setValue("orderingConvention", metadata.getString("orderingConvention"));
    }
}

void GribEncoder::setOceanCoordMetadata(const message::Metadata& metadata) {
    setOceanCoordMetadata(metadata, config_.getSubConfiguration("run"));
}
void GribEncoder::setOceanCoordMetadata(const message::Metadata& metadata, const eckit::Configuration& runConfig) {
    message::Metadata md = metadata;  // copy

    visitKeyValues(runConfig, [&](const std::string& k, const auto& v) { md.set(k, v); });

    // Set run-specific md
    setMarsKeys(*this, runConfig);

    setValue("date", md.getLong("startDate"));

    // setDomainDimensions
    // auto gls = lookUpLong(md, "globalSize");
    // setValue("numberOfDataPoints", md.getLong("globalSize")); // Readonly
    // setValue("numberOfValues", md.getLong("globalSize"));

    // Setting parameter ID
    setValue("paramId", md.getLong("param"));

    setValue("typeOfLevel", md.getString("typeOfLevel"));

    // Set ocean grid information
    setValue("unstructuredGridType", config_.getString("unstructured-grid-type"));

    setValue("unstructuredGridSubtype", md.getString("unstructuredGridSubtype"));

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

void GribEncoder::setMissing(const std::string& key) {
    encoder_->setMissing(key);
}

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

}  // namespace multio::action
