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

using message::glossary;
using message::Message;
using message::MetadataTypes;
using message::Peer;

using util::firstOf;
using util::lookUp;
using util::lookUpTranslate;
using util::withFirstOf;

namespace {
const std::map<const std::string, const std::int64_t> ops_to_code{
    {"instant", 0000}, {"average", 1000}, {"accumulate", 2000}, {"maximum", 3000}, {"minimum", 4000}, {"stddev", 5000}};

const std::map<const std::string, const std::int64_t> type_of_statistical_processing{
    {"average", 0}, {"accumulate", 1}, {"maximum", 2}, {"minimum", 3}, {"stddev", 6}};

const std::map<const std::string, const std::string> category_to_levtype{
    {"ocean-grid-coordinate", "oceanSurface"}, {"ocean-2d", "oceanSurface"}, {"ocean-3d", "oceanModelLevel"}};

const std::map<const std::string, const std::int64_t> type_of_generating_process{
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


std::tuple<std::int64_t, std::int64_t> getReferenceDateTime(const std::string& timeRef, const message::Metadata& in) {
    static std::unordered_map<std::string, std::tuple<std::string, std::string>> REF_TO_DATETIME_KEYS{
        {"start", {glossary().startDate, glossary().startTime}},
        {"previous", {glossary().previousDate, glossary().previousTime}},
        {"current", {glossary().currentDate, glossary().currentTime}},
    };

    auto search = REF_TO_DATETIME_KEYS.find(timeRef);

    return std::make_tuple(in.get<std::int64_t>(std::get<0>(search->second)),
                           in.get<std::int64_t>(std::get<1>(search->second)));
}


void tryMapStepToTimeAndCheckTime(message::Metadata& in) {
    const auto searchStartDate = in.find(glossary().startDate);
    const auto searchStartTime = in.find(glossary().startTime);
    const auto searchDataDate = in.find(glossary().dataDate);
    const auto searchDataTime = in.find(glossary().dataTime);
    const auto searchDate = in.find(glossary().date);
    const auto searchTime = in.find(glossary().time);

    bool hasStartDateTime = (searchStartDate != in.end() && searchStartTime != in.end());
    bool hasDataDateTime = (searchDataDate != in.end() && searchDataTime != in.end());
    bool hasDateTime = (searchDate != in.end() && searchTime != in.end());

    // std::cout << "tryMapStepToTimeAndCheckTime..." << std::endl;
    if (hasStartDateTime || hasDateTime || hasDataDateTime) {
        util::DateInts startDate;
        util::TimeInts startTime;

        if (hasStartDateTime) {
            startDate = util::toDateInts(searchStartDate->second.get<std::int64_t>());
            startTime = util::toTimeInts(searchStartTime->second.get<std::int64_t>());
        }
        else if (hasDataDateTime) {
            startDate = util::toDateInts(searchDataDate->second.get<std::int64_t>());
            startTime = util::toTimeInts(searchDataTime->second.get<std::int64_t>());
        }
        else if (hasDateTime) {
            startDate = util::toDateInts(searchDate->second.get<std::int64_t>());
            startTime = util::toTimeInts(searchTime->second.get<std::int64_t>() * 100);

            in.set("startDate", searchDate->second.get<std::int64_t>());
            in.set("startTime", searchTime->second.get<std::int64_t>() * 100);
        }

        // std::cout << "startDate: " << startDate.year << " " << startDate.month << " " << startDate.day << std::endl;
        // std::cout << "startTime: " << startTime.hour << " " << startTime.minute << " " << startTime.second  <<
        // std::endl;

        eckit::DateTime startDateTime(eckit::Date(startDate.year, startDate.month, startDate.day),
                                      eckit::Time(startTime.hour, startTime.minute, startTime.second));

        {
            const auto searchStep = in.find(glossary().step);
            const auto searchCurrentDate = in.find(glossary().currentDate);
            const auto searchCurrentTime = in.find(glossary().currentTime);
            if (searchStep != in.end() && (searchCurrentDate == in.end() || searchCurrentTime == in.end())) {
                const std::int64_t& step = searchStep->second.get<std::int64_t>();

                // IFS default step unit is hours
                auto currentDateTime = startDateTime + (step * 3600);

                in.set<std::int64_t>(glossary().currentDate, currentDateTime.date().yyyymmdd());
                in.set<std::int64_t>(glossary().currentTime, currentDateTime.time().hhmmss());
            }
        }


        const auto searchStepRange = in.find(glossary().stepRange);
        const auto searchStartStep = in.find(glossary().startStep);
        const auto searchEndStep = in.find(glossary().endStep);
        const auto searchCurrentDate = in.find(glossary().currentDate);
        const auto searchCurrentTime = in.find(glossary().currentTime);
        const auto searchPreviousDate = in.find(glossary().previousDate);
        const auto searchPreviousTime = in.find(glossary().previousTime);
        if ((searchStepRange != in.end() || (searchStartStep != in.end() && searchEndStep != in.end()))
            && (searchCurrentDate == in.end() || searchCurrentTime == in.end() || searchPreviousDate == in.end()
                || searchPreviousTime == in.end())) {

            std::int64_t stepStart;
            std::int64_t stepEnd;

            if (searchStepRange != in.end()) {
                const std::string& stepRange = searchStepRange->second.get<std::string>();
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
                stepStart = searchStartStep->second.get<std::int64_t>();
                stepEnd = searchEndStep->second.get<std::int64_t>();
            }

            // IFS default step unit is hours
            auto previousDateTime = startDateTime + (stepStart * 3600);

            in.set<std::int64_t>("previousDate", previousDateTime.date().yyyymmdd());
            in.set<std::int64_t>("previousTime", previousDateTime.time().hhmmss());

            // IFS default step unit is hours
            auto currentDateTime = startDateTime + (stepEnd * 3600);

            in.set<std::int64_t>("currentDate", currentDateTime.date().yyyymmdd());
            in.set<std::int64_t>("currentTime", currentDateTime.time().hhmmss());
        }
    }


    // Compute back from currentDate/Time + endStep/startStep
    const auto searchCurrentDate = in.find("currentDate");
    const auto searchCurrentTime = in.find("currentTime");
    if (searchCurrentDate == in.end() || searchCurrentTime == in.end()) {
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
    std::optional<std::int64_t> paramId{};
};


template <typename Dict>
QueriedMarsKeys setMarsKeys(GribEncoder& g, const Dict& md) {
    QueriedMarsKeys ret;

    // TODO we should be able to determine the type in the metadata and preserve
    // it Domain usually is always readonly withFirstOf(valueSetter(g, "domain"),
    // LookUpString(md, "domain"), LookUpString(md, "globalDomain"));
    const auto gridType = lookUp<std::string>(md, glossary().gridType)();
    const auto levtype = lookUp<std::string>(md, glossary().levtype)();
    const auto gribEdition = lookUp<std::string>(md, glossary().gribEdition)().value_or("2");

    const auto typeOfLevel = lookUp<std::string>(md, glossary().typeOfLevel)();
    if (!typeOfLevel) {
        const auto wam_levtype = lookUp<std::int64_t>(md, glossary().levtypeWam)();
        if (wam_levtype) {
            if (gribEdition == "1") {
                g.setValue("indicatorOfTypeOfLevel", *wam_levtype);
            }
            else if (levtype) {
                g.setValue("typeOfLevel", *levtype);
            }
        }
        else if (gridType && eckit::StringTools::lower(*gridType) != "healpix") {
            withFirstOf(valueSetter(g, "levtype"), lookUp<std::string>(md, glossary().levtype),
                        lookUp<std::string>(md, "indicatorOfTypeOfLevel"));
        }
        else if (gridType && levtype && eckit::StringTools::lower(*gridType) == "healpix" && *levtype != "o2d"
                 && *levtype != "o3d") {
            withFirstOf(valueSetter(g, "levtype"), lookUp<std::string>(md, glossary().levtype),
                        lookUp<std::string>(md, "indicatorOfTypeOfLevel"));
        }
        else if (!gridType) {
            withFirstOf(valueSetter(g, "levtype"), lookUp<std::string>(md, glossary().levtype),
                        lookUp<std::string>(md, "indicatorOfTypeOfLevel"));
        }

        if (levtype && (*levtype == "sfc")) {
            g.setValue("level", 0l);

            if (gribEdition == "2") {
                g.setMissing("scaleFactorOfFirstFixedSurface");
                g.setMissing("scaledValueOfFirstFixedSurface");
                g.setMissing("scaleFactorOfSecondFixedSurface");
                g.setMissing("scaledValueOfSecondFixedSurface");
            }
        }
        else {
            withFirstOf(valueSetter(g, "level"), lookUp<std::int64_t>(md, glossary().level),
                        lookUp<std::int64_t>(md, glossary().levelist));
        }
    }
    else {
        g.setValue("typeOfLevel", *typeOfLevel);
    }

    ret.paramId = firstOf(
        lookUp<std::int64_t>(md, glossary().paramId),
        lookUpTranslate<std::int64_t>(md, glossary().param));  // param might be a string, separated by . for GRIB1.
                                                               // String to std::int64_t convertion should get it right
    if (ret.paramId) {
        g.setValue(glossary().paramId, *ret.paramId);
    }
    withFirstOf(valueSetter(g, glossary().classKey), lookUp<std::string>(md, glossary().classKey),
                lookUp<std::string>(md, "marsClass"));
    withFirstOf(valueSetter(g, glossary().stream), lookUp<std::string>(md, glossary().stream),
                lookUp<std::string>(md, "marsStream"));
    withFirstOf(valueSetter(g, glossary().expver), lookUp<std::string>(md, glossary().expver),
                lookUp<std::string>(md, "experimentVersionNumber"));

    const auto dataset = lookUp<std::string>(md, "dataset")();
    if ((gribEdition == "2") && dataset) {
        withFirstOf(valueSetter(g, "subCentre"), lookUp<std::string>(md, "subCentre"));
        withFirstOf(valueSetter(g, "tablesVersion"), lookUp<std::int64_t>(md, "tablesVersion"));
        withFirstOf(valueSetter(g, "setLocalDefinition"), lookUp<std::int64_t>(md, "setLocalDefinition"));
        withFirstOf(valueSetter(g, "grib2LocalSectionNumber"), lookUp<std::int64_t>(md, "grib2LocalSectionNumber"));
        withFirstOf(valueSetter(g, "productionStatusOfProcessedData"),
                    lookUp<std::int64_t>(md, "productionStatusOfProcessedData"));

        g.setValue("dataset", *dataset);
        if (*dataset == "climate-dt") {
            withFirstOf(valueSetter(g, "activity"), lookUp<std::string>(md, "activity"));
            withFirstOf(valueSetter(g, "experiment"), lookUp<std::string>(md, "experiment"));
            withFirstOf(valueSetter(g, "generation"), lookUp<std::string>(md, "generation"));
            withFirstOf(valueSetter(g, "model"), lookUp<std::string>(md, "model"));
            withFirstOf(valueSetter(g, "realization"), lookUp<std::string>(md, "realization"));
        }
    }
    

    withFirstOf(valueSetter(g, "generatingProcessIdentifier"),
                lookUp<std::int64_t>(md, glossary().generatingProcessIdentifier));

    withFirstOf(valueSetter(g, "number"), lookUp<std::int64_t>(md, glossary().ensembleMember));
    withFirstOf(valueSetter(g, "numberOfForecastsInEnsemble"), lookUp<std::int64_t>(md, glossary().ensembleSize));
    
    withFirstOf(valueSetter(g, "methodNumber"), lookUp<std::int64_t>(md, "method-number"));
    withFirstOf(valueSetter(g, "systemNumber"), lookUp<std::int64_t>(md, "system-number"));

    ret.type = firstOf(lookUp<std::string>(md, glossary().type), lookUp<std::string>(md, glossary().marsType));
    if (ret.type) {
        g.setValue(glossary().type, *ret.type);
    }

    // Additional parameters passed through for spherical harmonics
    if (gridType) {
        if (*gridType == "sh") {
            withFirstOf(valueSetter(g, "complexPacking"), lookUp<std::int64_t>(md, glossary().complexPacking));
            withFirstOf(valueSetter(g, "pentagonalResolutionParameterJ"),
                        lookUp<std::int64_t>(md, glossary().pentagonalResolutionParameterJ),
                        lookUp<std::int64_t>(md, glossary().j));
            withFirstOf(valueSetter(g, "pentagonalResolutionParameterK"),
                        lookUp<std::int64_t>(md, glossary().pentagonalResolutionParameterK),
                        lookUp<std::int64_t>(md, glossary().k));
            withFirstOf(valueSetter(g, "pentagonalResolutionParameterM"),
                        lookUp<std::int64_t>(md, glossary().pentagonalResolutionParameterM),
                        lookUp<std::int64_t>(md, glossary().m));
                        
            withFirstOf(valueSetter(g, "subSetJ"), lookUp<std::int64_t>(md, glossary().subSetJ),
                        lookUp<std::int64_t>(md, glossary().js));
            withFirstOf(valueSetter(g, "subSetK"), lookUp<std::int64_t>(md, glossary().subSetK),
                        lookUp<std::int64_t>(md, glossary().ks));
            withFirstOf(valueSetter(g, "subSetM"), lookUp<std::int64_t>(md, glossary().subSetM),
                        lookUp<std::int64_t>(md, glossary().ms));
        }
        else if (*gridType == "regular_ll") {
            std::optional<std::int64_t> ni;
            std::optional<std::int64_t> nj;
            std::optional<double> north;
            std::optional<double> south;
            std::optional<double> west;
            std::optional<double> east;
            std::optional<double> westEastInc;
            std::optional<double> southNorthInc;
            if ((ni = lookUp<std::int64_t>(md, glossary().ni)()) && (nj = lookUp<std::int64_t>(md, glossary().nj)())
                && (north = lookUp<double>(md, glossary().north)()) && (south = lookUp<double>(md, glossary().south)())
                && (west = lookUp<double>(md, glossary().west)()) && (east = lookUp<double>(md, glossary().east)())
                && (westEastInc = lookUp<double>(md, glossary().westEastIncrement)())
                && (southNorthInc = lookUp<double>(md, glossary().southNorthIncrement)())) {
                std::int64_t scale = 0;
                if (gribEdition == "1") {
                    scale = 1000;
                }
                else if (gribEdition == "2") {
                    scale = 1000000;
                }
                g.setValue("Ni", *ni);
                g.setValue("Nj", *nj);
                g.setValue("latitudeOfFirstGridPoint", scale * *north);
                g.setValue("longitudeOfFirstGridPoint", scale * *west);
                g.setValue("latitudeOfLastGridPoint", scale * *south);
                g.setValue("longitudeOfLastGridPoint", scale * (*east - *westEastInc));
                g.setValue("iDirectionIncrement", scale * *westEastInc);
                g.setValue("jDirectionIncrement", scale * *southNorthInc);
            }
        }
        else if (eckit::StringTools::lower(*gridType) == "healpix") {
            withFirstOf(valueSetter(g, "Nside"), lookUp<std::int64_t>(md, glossary().nside));
            double logp = 45.0;
            // Note: Pedro told to use always this to avoid problems with milli and micro degrees
            g.setValue("longitudeOfFirstGridPointInDegrees", 45.0);
            withFirstOf(valueSetter(g, "orderingConvention"), lookUp<std::string>(md, glossary().orderingConvention));
        }
    }
    // TODO Remove Part of parameter mapping now
    // withFirstOf(valueSetter(g, "generatingProcessIdentifier"), lookUp<std::int64_t>(md,
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
    if (auto searchOverwrites = md.find("encoder-overwrites"); searchOverwrites != md.end()) {
        // TODO Refactor with visitor
        for (const auto& kv : searchOverwrites->second.get<message::Metadata>()) {
            if (g.hasKey(kv.first.value().c_str())) {
                kv.second.visit(eckit::Overloaded{
                    [](const auto& v) -> util::IfTypeOf<decltype(v), MetadataTypes::AllNested> {},
                        [&g, &kv](const auto& vec) -> util::IfTypeOf<decltype(vec), MetadataTypes::Lists> {
                            g.setValue(kv.first, vec);
                        },
                        [&g, &kv](const auto& v) -> util::IfTypeOf<decltype(v), MetadataTypes::NonNullScalars> {
                            g.setValue(kv.first, v);
                        },
                        [&g, &kv](const auto& v) -> util::IfTypeOf<decltype(v), MetadataTypes::Nulls> {
                            g.setValue(kv.first, 0);
                        }
                });
            }
        }
    }
}


void setEncodingSpecificFields(GribEncoder& g, const message::Metadata& md) {
    // TODO globalSize is expected to be set in md directly. nmuberOf* should be
    // readonly anyway... test removal..

    withFirstOf(valueSetter(g, glossary().missingValue), lookUp<double>(md, glossary().missingValue));
    withFirstOf(valueSetter(g, glossary().bitmapPresent), lookUp<bool>(md, glossary().bitmapPresent));
    withFirstOf(valueSetter(g, glossary().bitsPerValue), lookUp<std::int64_t>(md, glossary().bitsPerValue));
}

void setDateAndStatisticalFields(GribEncoder& g, const message::Metadata& in,
                                 const QueriedMarsKeys& queriedMarsFields) {
    message::Metadata md = in;  // Copy to allow modification

    auto gribEdition = lookUp<std::string>(md, "gribEdition")().value_or("2");
    // std::string forecastTimeKey = gribEdition == "2" ? "forecastTime" : "startStep";


    auto operation = lookUp<std::string>(md, "operation")();
    auto startStep = lookUp<std::int64_t>(md, "startStep")();
    auto endStep = lookUp<std::int64_t>(md, "endStep")();
    bool isTimeRange = (operation && (*operation != "instant"))
                    || (queriedMarsFields.type && *queriedMarsFields.type == "tpa")
                    || (endStep && startStep && (endStep != startStep));


    auto significanceOfReferenceTime = lookUp<std::int64_t>(md, "significanceOfReferenceTime")();
    if (!significanceOfReferenceTime) {
        if (auto searchEncoderOverwrites = md.find("encoder-overwrites"); searchEncoderOverwrites != md.end()) {
            const auto& overwrites = md.get<message::Metadata>("encoder-overwrites");
            significanceOfReferenceTime = lookUp<std::int64_t>(overwrites, "significanceOfReferenceTime")();
        }
    }
    if ((gribEdition == "2") && significanceOfReferenceTime) {
        g.setValue("significanceOfReferenceTime", *significanceOfReferenceTime);
    }

    tryMapStepToTimeAndCheckTime(md);

    std::string timeRef = std::invoke([&]() -> std::string {
        if (auto optTimeRef = lookUp<std::string>(md, "timeReference")(); optTimeRef) {
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

    auto currentDateTime = util::wrapDateTime({util::toDateInts(md.get<std::int64_t>(glossary().currentDate)),
                                               util::toTimeInts(md.get<std::int64_t>(glossary().currentTime))});

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
        auto previousDateTime = util::wrapDateTime({util::toDateInts(md.get<std::int64_t>(glossary().previousDate)),
                                                    util::toTimeInts(md.get<std::int64_t>(glossary().previousTime))});

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

        if (const auto timeIncrement = md.getOpt<std::int64_t>(glossary().timeIncrement); timeIncrement) {
            if (*timeIncrement != 0) {
                withFirstOf(valueSetter(g, "indicatorOfUnitForTimeIncrement"),
                            lookUp<std::int64_t>(md, glossary().indicatorOfUnitForTimeIncrement));
                g.setValue("timeIncrement", *timeIncrement);
            }
            else {
                g.setValue("indicatorOfUnitForTimeIncrement", 255);
                g.setValue("timeIncrement", 0);
            }
        }
        else if (const auto sampleIntervalInSeconds = md.getOpt<std::int64_t>(glossary().sampleIntervalInSeconds);
                 sampleIntervalInSeconds) {
            g.setValue("indicatorOfUnitForTimeIncrement", timeUnitCodes(util::TimeUnit::Second));
            g.setValue("timeIncrement", *sampleIntervalInSeconds);
        }
        else {
            g.setValue("indicatorOfUnitForTimeIncrement", timeUnitCodes(util::TimeUnit::Second));
            withFirstOf(valueSetter(g, "timeIncrement"),
                        lookUp<std::int64_t>(md, glossary().timeStep)());  // Nemo is currently sending timeStep
        }
    }


    auto dateOfAnalysis = firstOf(lookUp<std::int64_t>(md, glossary().dateOfAnalysis));
    auto timeOfAnalysis = firstOf(lookUp<std::int64_t>(md, glossary().timeOfAnalysis)).value_or(0);
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
    
    metadata.updateOverwrite(message::toMetadata(config_.getSubConfiguration("run").get()));

    auto queriedMarsFields = setMarsKeys(*this, metadata);
    if (queriedMarsFields.type) {
        setValue(glossary().typeOfGeneratingProcess, type_of_generating_process.at(*queriedMarsFields.type));
    }

    applyOverwrites(*this, metadata);
    setDateAndStatisticalFields(*this, metadata, queriedMarsFields);
    setEncodingSpecificFields(*this, metadata);

    // Setting parameter ID
    // auto paramInt = util::visitTranslate<std::int64_t>(metadata.get("param"));
    auto paramInt = metadata.get<std::int64_t>(glossary().paramId);
    // if (!paramInt) {
    //     std::ostringstream oss;
    //     oss << "GribEncoder::setOceanMetadata: Value for param can not be translated to int: ";
    //     oss << metadata.get("param");
    //     throw eckit::UserError(oss.str(), Here());
    // }
    if (paramInt / 1000 == 212) {
        // HACK! Support experimental averages.
        setValue(glossary().paramId, paramInt + 4000);
    }
    else {
        setValue(glossary().paramId, paramInt + ops_to_code.at(metadata.get<std::string>(glossary().operation)));
    }
    const auto& typeOfLevel = metadata.get<std::string>(glossary().typeOfLevel);
    setValue(glossary().typeOfLevel, typeOfLevel);
    if (typeOfLevel == "oceanModelLayer") {
        auto level = metadata.get<std::int64_t>(glossary().level);
        ASSERT(level > 0);
        setValue("scaledValueOfFirstFixedSurface", level - 1);
        setValue("scaledValueOfSecondFixedSurface", level);
        setValue("scaleFactorOfFirstFixedSurface", 0l);
        setValue("scaleFactorOfSecondFixedSurface", 0l);
    }
    if (typeOfLevel == "oceanModel") {
        auto level = metadata.get<std::int64_t>("level");
        ASSERT(level > 0);
        setValue("scaledValueOfFirstFixedSurface", level);
        setValue("scaleFactorOfFirstFixedSurface", 0l);
    }

    std::string gridType;
    const auto searchGridType = metadata.find(glossary().gridType);
    if (searchGridType != metadata.end() && searchGridType->second.get<std::string>() == "unstructured_grid") {
        if (auto searchGridType = metadata.find("unstructuredGridType"); searchGridType != metadata.end()) {
            setValue(glossary().unstructuredGridType, searchGridType->second.template get<std::string>());
        }
        else {
            setValue(glossary().unstructuredGridType, config_.getString("unstructured-grid-type"));
        }

        if (auto searchGridSubtype = metadata.find(glossary().unstructuredGridSubtype);
            searchGridSubtype != metadata.end()) {
            setValue("unstructuredGridSubtype", searchGridSubtype->second.template get<std::string>());
        }

        if (auto searchGridUUID = metadata.find("uuidOfHGrid"); searchGridUUID != metadata.end()) {
            setValue("uuidOfHGrid", searchGridUUID->second.template get<std::string>());
        }
        else {
            eckit::Log::warning() << "Ocean grid UUID not available during encoding!" << std::endl;
        }
    }
    else if (eckit::StringTools::lower(gridType) == "healpix") {
        long Nside = metadata.get<std::int64_t>("Nside");
        setValue("Nside", Nside);
        double logp = 45.0;
        // Note: Pedro told to use always this to avoid problems with milli and micro degrees
        setValue("longitudeOfFirstGridPointInDegrees", logp);
        setValue("orderingConvention", metadata.get<std::string>("orderingConvention"));
    }
}

void GribEncoder::setOceanCoordMetadata(const message::Metadata& metadata) {
    setOceanCoordMetadata(metadata, config_.getSubConfiguration("run"));
}
void GribEncoder::setOceanCoordMetadata(const message::Metadata& metadata, const eckit::Configuration& runConfig) {
    auto md = metadata;  // copy

    md.updateOverwrite(message::toMetadata(runConfig.get()));

    // Set run-specific md
    setMarsKeys(*this, md);

    setValue(glossary().date, md.get<std::int64_t>(glossary().startDate));

    // setDomainDimensions
    // auto gls = lookUp<std::int64_t>(md, glossary().globalSize);
    // setValue("numberOfDataPoints", md.get<std::int64_t>(glossary().globalSize));
    // setValue("numberOfValues", md.get<std::int64_t>(glossary().globalSize));

    // Setting parameter ID
    // auto paramInt = util::visitTranslate<std::int64_t>(md.get(glossary().param));
    auto paramInt = md.get<std::int64_t>(glossary().paramId);
    // if (!paramInt) {
    //     std::ostringstream oss;
    //     oss << "GribEncoder::setOceanCoordMetadata: Value for param can not be translated to int: ";
    //     oss << md.get("param");
    //     throw eckit::UserError(oss.str(), Here());
    // }
    setValue(glossary().paramId, paramInt);

    setValue(glossary().typeOfLevel, md.get<std::string>(glossary().typeOfLevel));

    // Set ocean grid information
    setValue(glossary().unstructuredGridType, config_.getString("unstructured-grid-type"));

    const auto& gridSubtype
        = md.get<std::string>("gridSubtype");  // TO BE REMOVED IN THE FUTURE - should be named unstructuredGridType
    setValue(glossary().unstructuredGridSubtype, gridSubtype.substr(0, 1));

    const auto& gridUID = md.get<std::string>(glossary().uuidOfHGrid);
    setValue(glossary().uuidOfHGrid, gridUID);

    // Set encoding for missing value support
    setValue(glossary().bitmapPresent, false);
    setValue(glossary().bitsPerValue, md.get<std::int64_t>(glossary().bitsPerValue));
}


void GribEncoder::initEncoder() {
    encoder_ = template_.duplicate();
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

    return Message{Message::Header{Message::Tag::Field, Peer{msg.source().group()}, Peer{msg.destination()}},
                   std::move(buf)};
}


message::Message GribEncoder::setFieldValues(const double* values, size_t count) {
    encoder_->setDataValues(values, count);

    eckit::Buffer buf{this->encoder_->length()};
    encoder_->write(buf);

    return Message{Message::Header{Message::Tag::Field, Peer{}, Peer{}}, std::move(buf)};
}

message::Message GribEncoder::setFieldValues(const float* values, size_t count) {
    encoder_->setDataValues(values, count);

    eckit::Buffer buf{this->encoder_->length()};
    encoder_->write(buf);

    return Message{Message::Header{Message::Tag::Field, Peer{}, Peer{}}, std::move(buf)};
}


void GribEncoder::print(std::ostream& os) const {
    os << "GribEncoder(config=" << config_ << ")";
};

}  // namespace multio::action
