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
#include <unordered_map>
#include <functional>

#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"
#include "eckit/types/DateTime.h"
#include "eckit/utils/MD5.h"
#include "eckit/utils/StringTools.h"
#include "eckit/utils/Translator.h"


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
    // // if (search == REF_TO_DATETIME_KEYS.end()) {
    // //     std::ostringstream oss;
    // //     oss << "getReferenceDateTime: Time reference \"" << timeRef << "\" can not be mapped";
    //     throw EncodeGrib2Exception(oss.str(), Here());
    // }

    return std::make_tuple((std::int64_t)in.getLong(std::get<0>(search->second)),
                           (std::int64_t)in.getLong(std::get<1>(search->second)));
}


void tryMapStepToTimeAndCheckTime(eckit::LocalConfiguration& in) {

    bool hasStartDateTime = (in.has("startDate") && in.has("startTime"));
    bool hasDateTime = (in.has("date") && in.has("time"));

    // std::cout << "tryMapStepToTimeAndCheckTime..." << std::endl;
    if (hasStartDateTime || hasDateTime) {
        util::DateInts startDate;
        util::TimeInts startTime;

        if (hasStartDateTime) {
            startDate = util::toDateInts(in.getLong("startDate"));
            startTime = util::toTimeInts(in.getLong("startTime"));
        }
        else if (hasDateTime) {
            startDate = util::toDateInts(in.getLong("date"));
            startTime = util::toTimeInts(in.getLong("time"));

            in.set("startDate", in.getLong("date"));
            in.set("startTime", in.getLong("time"));
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

            in.set("currentDate", (std::int64_t)currentDateTime.date().yyyymmdd());
            in.set("currentTime", (std::int64_t)currentDateTime.time().hhmmss());
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

            in.set("previousDate", (std::int64_t)previousDateTime.date().yyyymmdd());
            in.set("previousTime", (std::int64_t)previousDateTime.time().hhmmss());

            // IFS default step unit is hours
            auto currentDateTime = startDateTime + (stepEnd * 3600);

            in.set("currentDate", (std::int64_t)currentDateTime.date().yyyymmdd());
            in.set("currentTime", (std::int64_t)currentDateTime.time().hhmmss());
        }
    }

    // std::cout << "tryMapStepToTimeAndCheckTime done" << std::endl;

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
    const auto wam_levtype = lookUpLong(md, "levtype_wam");
    if (wam_levtype) {
        g.setValue("indicatorOfTypeOfLevel", wam_levtype);
    }
    else if (md.has("gridType") && eckit::StringTools::lower(md.getString("gridType")) != "healpix") {
        withFirstOf(valueSetter(g, "levtype"), LookUpString(md, "levtype"), LookUpString(md, "indicatorOfTypeOfLevel"));
    }
    else if (md.has("gridType") && eckit::StringTools::lower(md.getString("gridType")) == "healpix"
             && md.getString("levtype") != "o2d" && md.getString("levtype") != "o3d") {
        withFirstOf(valueSetter(g, "levtype"), LookUpString(md, "levtype"), LookUpString(md, "indicatorOfTypeOfLevel"));
    }
    else if (!md.has("gridType")) {
        withFirstOf(valueSetter(g, "levtype"), LookUpString(md, "levtype"), LookUpString(md, "indicatorOfTypeOfLevel"));
    }

    if (md.has("levtype") && (md.getString("levtype") == "sfc")) {
        g.setValue("level", (long)0);
        g.setValue("scaleFactorOfFirstFixedSurface", "MISSING");
        g.setValue("scaledValueOfFirstFixedSurface", "MISSING");
        g.setValue("scaleFactorOfSecondFixedSurface", "MISSING");
        g.setValue("scaleFactorOfSecondFixedSurface", "MISSING");
    } 
    else {
        withFirstOf(valueSetter(g, "level"), LookUpLong(md, "level"), LookUpLong(md, "levelist"));
    }

    // withFirstOf(valueSetter(g, "date"), LookUpLong(md, "date"), LookUpLong(md, "dataDate"));
    // withFirstOf(valueSetter(g, "time"), LookUpLong(md, "time"), LookUpLong(md, "dataTime"));
    // withFirstOf(valueSetter(g, "step"), LookUpLong(md, "step"), LookUpLong(md, "startStep"));

    std::optional<std::string> paramId{firstOf(
        LookUpString(md, "paramId"), LookUpString(md, "param"))};  // param might be a string, separated by . for GRIB1.
                                                                   // String to long convertion should get it right


    if (paramId) {
        g.setValue("paramId", eckit::Translator<std::string, long>{}(*paramId));
    }

    if (md.has("dataset")) {
        withFirstOf(valueSetter(g, "tablesVersion"), LookUpLong(md, "tablesVersion"));
        withFirstOf(valueSetter(g, "setLocalDefinition"), LookUpLong(md, "setLocalDefinition"));
        withFirstOf(valueSetter(g, "grib2LocalSectionNumber"), LookUpLong(md, "grib2LocalSectionNumber"));
        withFirstOf(valueSetter(g, "productionStatusOfProcessedData"), LookUpLong(md, "productionStatusOfProcessedData"));
        withFirstOf(valueSetter(g, "dataset"), LookUpString(md, "dataset"));
        withFirstOf(valueSetter(g, "activity"), LookUpString(md, "activity"));
        withFirstOf(valueSetter(g, "experiment"), LookUpString(md, "experiment"));
        withFirstOf(valueSetter(g, "generation"), LookUpString(md, "generation"));
        withFirstOf(valueSetter(g, "model"), LookUpString(md, "model"));
        withFirstOf(valueSetter(g, "realization"), LookUpString(md, "realization"));
        withFirstOf(valueSetter(g, "resolution"), LookUpString(md, "resolution"));
    }

    withFirstOf(valueSetter(g, "class"), LookUpString(md, "class"), LookUpString(md, "marsClass"));
    withFirstOf(valueSetter(g, "stream"), LookUpString(md, "stream"), LookUpString(md, "marsStream"));

    withFirstOf(valueSetter(g, "expver"), LookUpString(md, "expver"), LookUpString(md, "experimentVersionNumber"));
    withFirstOf(valueSetter(g, "number"), LookUpLong(md, "ensemble-member"));
    withFirstOf(valueSetter(g, "numberOfForecastsInEnsemble"), LookUpLong(md, "ensemble-size"));

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
        else if (eckit::StringTools::lower(md.getString("gridType")) == "healpix") {
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
    // auto gls = lookUpLong(md, "globalSize");
    // withFirstOf(valueSetter(g, "numberOfDataPoints"), gls); // Readonly
    // withFirstOf(valueSetter(g, "numberOfValues"), gls);

    withFirstOf(valueSetter(g, "missingValue"), LookUpDouble(md, "missingValue"));
    withFirstOf(valueSetter(g, "bitmapPresent"), LookUpBool(md, "bitmapPresent"));
    withFirstOf(valueSetter(g, "bitsPerValue"), LookUpLong(md, "bitsPerValue"));
}

void setDateAndStatisticalFields(GribEncoder& g, const eckit::LocalConfiguration& in,
                                 const QueriedMarsKeys& queriedMarsFields) {
    eckit::LocalConfiguration md = in;  // Copy to allow modification

    std::string gribEdition = md.getString("gribEdition", "2");
    // std::string forecastTimeKey = gribEdition == "2" ? "forecastTime" : "startStep";


    auto operation = lookUpString(md, "operation");
    bool isTimeRange
        = (operation && (*operation != "instant")) || (queriedMarsFields.type && *queriedMarsFields.type == "tpa");


    auto significanceOfReferenceTime = lookUpLong(md, "significanceOfReferenceTime");
    if (!significanceOfReferenceTime) {
        if (md.has("encoder-overwrites")) {
            auto overwrites = md.getSubConfiguration("encoder-overwrites");
            significanceOfReferenceTime = lookUpLong(overwrites, "significanceOfReferenceTime");
        }
    }
    if (significanceOfReferenceTime) {
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
                if (significanceOfReferenceTime && (*significanceOfReferenceTime == 2)) {
                    isReferingToStart = false;
                    g.setValue("stepUnits", (long)0);
                    g.setValue("startStep", (long)0);
                    if (gribEdition == "2") {
                        g.setValue("indicatorOfUnitOfTimeRange", (long)0);
                        g.setValue("forecastTime", (long)0);
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

    auto refDateTime = getReferenceDateTime(timeRef, md);
    // g.setValue("dataDate", (long)std::get<0>(refDateTime));
    // g.setValue("dataTime", (long)std::get<1>(refDateTime));

    auto refDate = util::toDateInts(std::get<0>(refDateTime));
    g.setValue("year", (long)refDate.year);
    g.setValue("month", (long)refDate.month);
    g.setValue("day", (long)refDate.day);

    auto refTime = util::toTimeInts(std::get<1>(refDateTime));
    g.setValue("hour", (long)refTime.hour);
    g.setValue("minute", (long)refTime.minute);
    g.setValue("second", (long)refTime.second);

    auto currentDate = util::toDateInts(md.getLong("currentDate"));
    auto currentTime = util::toTimeInts(md.getLong("currentTime"));
    if (!isTimeRange) {
        if (timeRef == std::string("start")) {
            // Compute diff to current time in some appropriate unit
            g.setValue("stepUnits", (long)timeUnitCodes(util::TimeUnit::Hour));
            g.setValue("startStep",
                       (long)util::dateTimeDiffInSeconds(currentDate, currentTime, refDate, refTime) / 3600);
            // util::DateTimeDiff diff = util::dateTimeDiff(currentDate, currentTime, refDate, refTime);
            // g.setValue("stepUnits", (long)timeUnitCodes(diff.unit));
            // g.setValue("startStep", (long)diff.diff);
        }
        else {
            g.setValue("stepUnits", (long)0);
            g.setValue("startStep", (long)0);
        }
    }
    else {
        auto previousDate = util::toDateInts(md.getLong("previousDate"));
        auto previousTime = util::toTimeInts(md.getLong("previousTime"));
        if (timeRef == std::string("start")) {
            // Compute diff to current time in some appropriate unit
            g.setValue("stepUnits", (long)timeUnitCodes(util::TimeUnit::Hour));
            g.setValue("startStep",
                       (long)util::dateTimeDiffInSeconds(previousDate, previousTime, refDate, refTime) / 3600);
            // util::DateTimeDiff diff = util::dateTimeDiff(previousDate, previousTime, refDate, refTime);
            // g.setValue("stepUnits", (long)timeUnitCodes(diff.unit));
            // g.setValue("startStep", (long)diff.diff);


            // Set endStep to please MARS
            g.setValue("endStep", (long)util::dateTimeDiffInSeconds(currentDate, currentTime, refDate, refTime) / 3600);
            // util::DateTimeDiff diffEnd = util::dateTimeDiff(currentDate, currentTime, refDate, refTime);
            // g.setValue("endStep", (long)diffEnd.diff);
        }
        else {
            // No forecast time is used
            g.setValue("stepUnits", (long)0);
            g.setValue("startStep", (long)0);
            if (gribEdition == "2") {
                g.setValue("indicatorOfUnitOfTimeRange", (long)0);
                g.setValue("forecastTime", (long)0);
            }


            // Set endStep to please MARS
            g.setValue("endStep",
                       (long)util::dateTimeDiffInSeconds(currentDate, currentTime, previousDate, previousTime) / 3600);
            // util::DateTimeDiff diffEnd = util::dateTimeDiff(currentDate, currentTime, previousDate, previousTime);
            // g.setValue("endStep", (long)diffEnd.diff);
        }


        // Now just deal with GRIB2
        g.setValue("yearOfEndOfOverallTimeInterval", (long)currentDate.year);
        g.setValue("monthOfEndOfOverallTimeInterval", (long)currentDate.month);
        g.setValue("dayOfEndOfOverallTimeInterval", (long)currentDate.day);
        g.setValue("hourOfEndOfOverallTimeInterval", (long)currentTime.hour);
        g.setValue("minuteOfEndOfOverallTimeInterval", (long)currentTime.minute);
        g.setValue("secondOfEndOfOverallTimeInterval", (long)currentTime.second);

        g.setValue("indicatorOfUnitForTimeRange", (long)timeUnitCodes(util::TimeUnit::Hour));
        g.setValue("lengthOfTimeRange",
                   (long)util::dateTimeDiffInSeconds(currentDate, currentTime, previousDate, previousTime) / 3600);
        // util::DateTimeDiff lengthTimeRange = util::dateTimeDiff(currentDate, currentTime, previousDate,
        // previousTime); g.setValue("indicatorOfUnitForTimeRange", (long)timeUnitCodes(lengthTimeRange.unit));
        // g.setValue("lengthOfTimeRange", (long)lengthTimeRange.diff);

        if (operation) {
            static const std::map<const std::string, const std::int64_t> TYPE_OF_STATISTICAL_PROCESSING{
                {"average", 0}, {"accumulate", 1}, {"maximum", 2}, {"minimum", 3}, {"stddev", 6}};
            if (auto searchStat = TYPE_OF_STATISTICAL_PROCESSING.find(*operation);
                searchStat != TYPE_OF_STATISTICAL_PROCESSING.end()) {
                g.setValue("typeOfStatisticalProcessing", (long)searchStat->second);
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
        g.setValue("typeOfTimeIncrement", (long)(timeRef == "start" ? 2 : 1));

        // auto sampleIntervalUnit = util::TimeUnit::Second;
        // if (md.has("sampleIntervalUnit")) {
        //     auto sampleIntervalUnitStr = md.getString("sampleIntervalUnit");
        //     auto sampleIntervalUnitMb = util::timeUnitFromString(sampleIntervalUnitStr);
        //     if (!sampleIntervalUnitMb) {
        //         std::ostringstream oss;
        //         oss << "setDateAndStatisticalFields - Value for passed metadatakey \"sampleIntervalUnit\": "
        //             << sampleIntervalUnitStr << " can not be parsed to a valid unit (Y,m,d,H,M,S). ";
        //         throw eckit::UserError(oss.str(), Here());
        //     }
        //     else {
        //         sampleIntervalUnit = *sampleIntervalUnitMb;
        //     }
        // }
        // g.setValue("indicatorOfUnitForTimeIncrement", (long)timeUnitCodes(sampleIntervalUnit));
        g.setValue("indicatorOfUnitForTimeIncrement", (long)timeUnitCodes(util::TimeUnit::Second));
        withFirstOf(valueSetter(g, "timeIncrement"), LookUpLong(md, "sampleIntervalInSeconds"),
                    LookUpLong(md, "timeStep"));  // Nemo is currently sending timeStep
    }


    auto dateOfAnalysis = firstOf(LookUpLong(md, "date-of-analysis"));
    if (dateOfAnalysis) {
        auto date = util::toDateInts(*dateOfAnalysis);
        g.setValue("yearOfAnalysis", (long)date.year);
        g.setValue("monthOfAnalysis", (long)date.month);
        g.setValue("dayOfAnalysis", (long)date.day);
    }

    auto timeOfAnalysis = firstOf(LookUpLong(md, "time-of-analysis"));
    if (timeOfAnalysis) {
        auto time = util::toTimeInts(*timeOfAnalysis);
        g.setValue("hourOfAnalysis", (long)time.hour);
        g.setValue("minuteOfAnalysis", (long)time.minute);
        g.setValue("secondOfAnalysis", (long)time.second);
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
    }
    else {
        setValue("paramId", metadata.getLong("param") + ops_to_code.at(metadata.getString("operation")));
    }
    setValue("typeOfLevel", metadata.getString("typeOfLevel"));
    if (metadata.getString("category") == "ocean-3d") {
        auto level = metadata.getLong("level");
        ASSERT(level > 0);
        setValue("scaledValueOfFirstFixedSurface", level - 1);
        setValue("scaledValueOfSecondFixedSurface", level);
    }

    std::string gridType;
    const auto hasGridType = metadata.get("gridType", gridType);
    if (eckit::StringTools::lower(gridType) != "healpix") {
        // Set ocean grid information
        setValue("unstructuredGridType", config_.getString("grid-type"));

        const auto& gridSubtype = metadata.getString("gridSubtype");
        setValue("unstructuredGridSubtype", gridSubtype.substr(0, 1));

        const auto& gridUID = metadata.getString("uuidOfHGrid");
        setValue("uuidOfHGrid", gridUID);
    }

    if (metadata.has("dataset")) {
        withFirstOf(valueSetter(*this, "tablesVersion"), LookUpLong(metadata, "tablesVersion"));
        withFirstOf(valueSetter(*this, "setLocalDefinition"), LookUpLong(metadata, "setLocalDefinition"));
        withFirstOf(valueSetter(*this, "grib2LocalSectionNumber"), LookUpLong(metadata, "grib2LocalSectionNumber"));
        withFirstOf(valueSetter(*this, "productionStatusOfProcessedData"), LookUpLong(metadata, "productionStatusOfProcessedData"));
        withFirstOf(valueSetter(*this, "dataset"), LookUpString(metadata, "dataset"));
        withFirstOf(valueSetter(*this, "activity"), LookUpString(metadata, "activity"));
        withFirstOf(valueSetter(*this, "experiment"), LookUpString(metadata, "experiment"));
        withFirstOf(valueSetter(*this, "generation"), LookUpString(metadata, "generation"));
        withFirstOf(valueSetter(*this, "model"), LookUpString(metadata, "model"));
        withFirstOf(valueSetter(*this, "realization"), LookUpString(metadata, "realization"));
        withFirstOf(valueSetter(*this, "resolution"), LookUpString(metadata, "resolution"));
        withFirstOf(valueSetter(*this, "class"), LookUpString(metadata, "class"), LookUpString(metadata, "marsClass"));
        withFirstOf(valueSetter(*this, "stream"), LookUpString(metadata, "stream"), LookUpString(metadata, "marsStream"));
        withFirstOf(valueSetter(*this, "expver"), LookUpString(metadata, "expver"), LookUpString(metadata, "experimentVersionNumber"));
    }
}

void GribEncoder::setOceanCoordMetadata(const message::Metadata& metadata) {
    setOceanCoordMetadata(metadata, config_.getSubConfiguration("run"));
}
void GribEncoder::setOceanCoordMetadata(const message::Metadata& md, const eckit::Configuration& runConfig) {
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

}  // namespace multio::action
