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
#include <unordered_set>

#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"
#include "eckit/types/DateTime.h"
#include "eckit/utils/MD5.h"
#include "eckit/utils/StringTools.h"
#include "eckit/utils/Translator.h"
#include "eckit/value/Value.h"


#include "multio/LibMultio.h"
#include "multio/util/DateTime.h"
#include "multio/util/Environment.h"
#include "multio/util/Metadata.h"
#include "multio/util/Substitution.h"

#include "multio/util/PrecisionTag.h"

#define DIGEST_LENGTH MD5_DIGEST_LENGTH

namespace multio::action::encode {

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
    {"average", 0},    {"accumulate", 1}, {"maximum", 2},           {"minimum", 3},
    {"difference", 4}, {"stddev", 6},     {"inverse-difference", 8}};

const std::map<const std::string, const std::string> category_to_levtype{
    {"ocean-grid-coordinate", "oceanSurface"}, {"ocean-2d", "oceanSurface"}, {"ocean-3d", "oceanModelLevel"}};

const std::map<const std::string, const long> type_of_generating_process{{"an", 0}, {"4v", 0}, {"fc", 2},
                                                                         {"cf", 4}, {"pf", 4}, {"tpa", 12}};


const std::unordered_set<std::string> types_with_time_reference_offset{"fc", "fcmean", "cf", "pf", "4v"};

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
        {"start", {dm::legacy::StartDate, dm::legacy::StartTime}},
        {"previous", {dm::legacy::PreviousDate, dm::legacy::PreviousTime}},
        {"current", {dm::legacy::CurrentDate, dm::legacy::CurrentTime}},
    };

    auto search = REF_TO_DATETIME_KEYS.find(timeRef);

    return std::make_tuple(in.get<std::int64_t>(std::get<0>(search->second)),
                           in.get<std::int64_t>(std::get<1>(search->second)));
}


void tryMapStepToTimeAndCheckTime(message::Metadata& in) {
    const auto searchStartDate = in.find(dm::legacy::StartDate);
    const auto searchStartTime = in.find(dm::legacy::StartTime);
    const auto searchDataDate = in.find(dm::legacy::DataDate);
    const auto searchDataTime = in.find(dm::legacy::DataTime);
    const auto searchDate = in.find(dm::legacy::Date);
    const auto searchTime = in.find(dm::legacy::Time);

    bool hasStartDateTime = (searchStartDate != in.end() && searchStartTime != in.end());
    bool hasDataDateTime = (searchDataDate != in.end() && searchDataTime != in.end());
    bool hasDateTime = (searchDate != in.end() && searchTime != in.end());

    if (hasStartDateTime || hasDateTime || hasDataDateTime) {
        util::DateInts startDate;
        util::TimeInts startTime;

        if (hasStartDateTime) {
            startDate = util::toDateInts(searchStartDate->second.get<std::int64_t>());
            startTime = util::toTimeInts(searchStartTime->second.get<std::int64_t>());
        }
        else if (hasDataDateTime) {
            startDate = util::toDateInts(searchDataDate->second.get<std::int64_t>());
            startTime = util::toTimeInts(searchDataTime->second.get<std::int64_t>() * 100);

            in.set("startDate", searchDate->second.get<std::int64_t>());
            in.set("startTime", searchTime->second.get<std::int64_t>() * 100);
        }
        else if (hasDateTime) {
            startDate = util::toDateInts(searchDate->second.get<std::int64_t>());
            startTime = util::toTimeInts(searchTime->second.get<std::int64_t>() * 100);

            in.set("startDate", searchDate->second.get<std::int64_t>());
            in.set("startTime", searchTime->second.get<std::int64_t>() * 100);
        }

        eckit::DateTime startDateTime(eckit::Date(startDate.year, startDate.month, startDate.day),
                                      eckit::Time(startTime.hour, startTime.minute, startTime.second));

        {
            const auto searchStep = in.find(dm::legacy::Step);
            const auto searchCurrentDate = in.find(dm::legacy::CurrentDate);
            const auto searchCurrentTime = in.find(dm::legacy::CurrentTime);
            if (searchStep != in.end() && (searchCurrentDate == in.end() || searchCurrentTime == in.end())) {
                const std::int64_t& step = searchStep->second.get<std::int64_t>();

                // IFS default step unit is hours
                auto currentDateTime = startDateTime + (step * 3600);

                in.set<std::int64_t>(dm::legacy::CurrentDate, currentDateTime.date().yyyymmdd());
                in.set<std::int64_t>(dm::legacy::CurrentTime, currentDateTime.time().hhmmss());
            }
        }


        const auto searchStepRange = in.find(dm::legacy::StepRange);
        const auto searchStartStep = in.find(dm::legacy::StartStep);
        const auto searchEndStep = in.find(dm::legacy::EndStep);
        const auto searchCurrentDate = in.find(dm::legacy::CurrentDate);
        const auto searchCurrentTime = in.find(dm::legacy::CurrentTime);
        const auto searchPreviousDate = in.find(dm::legacy::PreviousDate);
        const auto searchPreviousTime = in.find(dm::legacy::PreviousTime);
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

std::string getUnstructuredGridType(const eckit::LocalConfiguration& config) {
    return config.getString("unstructured-grid-type");
}

}  // namespace

GribEncoder::GribEncoder(std::unique_ptr<metkit::codes::CodesHandle> handle, const eckit::LocalConfiguration& config) :
    template_{std::move(handle)}, encoder_{nullptr}, config_{config} /*, encodeBitsPerValue_(config)*/ {}

struct QueriedMarsKeys {
    std::optional<std::string> type{};
    std::optional<std::int64_t> paramId{};
};

void setLevelUnrelatedTypeOfLevel(GribEncoder& g, const std::string& typeOfLevel, long level) {
    g.setValue("typeOfLevel", typeOfLevel);
}

void setLayerTypeOfLevel(GribEncoder& g, const std::string& typeOfLevel, long level) {
    g.setValue("typeOfLevel", typeOfLevel);
    g.setValue("scaleFactorOfFirstFixedSurface", 0);
    g.setValue("scaledValueOfFirstFixedSurface", level - 1);
    g.setValue("scaleFactorOfSecondFixedSurface", 0);
    g.setValue("scaledValueOfSecondFixedSurface", level);
}

void setSoilLayerTypeOfLevel(GribEncoder& g, const std::string& typeOfLevel, long level) {
    g.setValue("typeOfLevel", typeOfLevel);

    g.setValue("typeOfFirstFixedSurface", 151);
    g.setValue("typeOfSecondFixedSurface", 151);

    g.setValue("scaleFactorOfFirstFixedSurface", 0);
    g.setValue("scaleFactorOfSecondFixedSurface", 0);

    g.setValue("scaledValueOfFirstFixedSurface", level - 1);
    g.setValue("scaledValueOfSecondFixedSurface", level);
}

void setMissingFixedSurface(GribEncoder& g, const std::string& typeOfLevel, long level) {
    g.setValue("typeOfLevel", typeOfLevel);

    g.setMissing(dm::legacy::ScaleFactorOfFirstFixedSurface);
    g.setMissing(dm::legacy::ScaledValueOfFirstFixedSurface);
    g.setMissing(dm::legacy::ScaleFactorOfSecondFixedSurface);
    g.setMissing(dm::legacy::ScaledValueOfSecondFixedSurface);
}

using TypeOfLevelSetter = std::function<void(GribEncoder&, const std::string&, long)>;

const std::map<std::string, TypeOfLevelSetter> typeOfLevelSetters{
    {"snowLayer", &setLayerTypeOfLevel},
    {"soilLayer", &setSoilLayerTypeOfLevel},
    {"seaIceLayer", &setLayerTypeOfLevel},
    {"mediumCloudLayer", &setLevelUnrelatedTypeOfLevel},
    {"lowCloudLayer", &setLevelUnrelatedTypeOfLevel},
    {"highCloudLayer", &setLevelUnrelatedTypeOfLevel},
    {"meanSea", &setLevelUnrelatedTypeOfLevel},
    {"iceLayerOnWater", &setMissingFixedSurface},
};

template <typename Dict>
QueriedMarsKeys setMarsKeys(GribEncoder& g, const Dict& md) {
    QueriedMarsKeys ret;

    // TODO we should be able to determine the type in the metadata and preserve
    // it Domain usually is always readonly withFirstOf(valueSetter(g, "domain"),
    // LookUpString(md, "domain"), LookUpString(md, "globalDomain"));
    const auto gridType = lookUp<std::string>(md, dm::legacy::GridType)();
    const auto levtype = lookUp<std::string>(md, dm::legacy::Levtype)();
    const auto gribEdition = lookUp<std::string>(md, dm::legacy::GribEdition)().value_or("2");

    if ((gribEdition == "2") && (gridType != "sh")) {
        withFirstOf(valueSetter(g, dm::legacy::SetPackingType), lookUp<std::string>(md, dm::legacy::SetPackingType));
    }

    auto localDefinitionNumber = lookUp<std::int64_t>(md, dm::legacy::LocalDefinitionNumber)();
    auto grib2LocalSectionNumber = lookUp<std::int64_t>(md, dm::legacy::Grib2LocalSectionNumber)();


    if (gribEdition == "2") {
        withFirstOf(valueSetter(g, dm::legacy::SubCentre), lookUp<std::int64_t>(md, dm::legacy::SubCentre));
        withFirstOf(valueSetter(g, dm::legacy::TablesVersion), lookUp<std::int64_t>(md, dm::legacy::TablesVersion));
        if (localDefinitionNumber || grib2LocalSectionNumber) {
            withFirstOf(valueSetter(g, dm::legacy::LocalTablesVersion),
                        lookUp<std::int64_t>(md, dm::legacy::LocalTablesVersion));
            g.setValue(dm::legacy::SetLocalDefinition, 1);
            withFirstOf(valueSetter(g, dm::legacy::LocalDefinitionNumber), localDefinitionNumber);
            withFirstOf(valueSetter(g, dm::legacy::Grib2LocalSectionNumber), grib2LocalSectionNumber);

            if (auto extraLocalDef = lookUp<std::int64_t>(md, dm::legacy::ExtraLocalSectionNumber)(); extraLocalDef) {
                g.setValue(dm::legacy::ExtraLocalSectionNumber, *extraLocalDef);
            }
            else {
                g.setValue(dm::legacy::DeleteExtraLocalSection, 1);
            }
        }

        withFirstOf(valueSetter(g, dm::legacy::ProductDefinitionTemplateNumber),
                    lookUp<std::int64_t>(md, dm::legacy::ProductDefinitionTemplateNumber));
    }

    const auto typeOfLevel = lookUp<std::string>(md, dm::legacy::TypeOfLevel)();
    if (typeOfLevel) {
        if (auto searchTOLSetter = typeOfLevelSetters.find(*typeOfLevel); searchTOLSetter != typeOfLevelSetters.end()) {
            const auto level = lookUp<std::int64_t>(md, dm::legacy::Level)();
            const auto levelist = lookUp<std::int64_t>(md, dm::legacy::Levelist)();

            if (!level && !levelist) {
                std::ostringstream oss;
                oss << "setMarsKeys - field " << lookUp<std::string>(md, dm::legacy::ParamId)().value_or("???")
                    << " with typeOfLevel " << *typeOfLevel << ", but no level information!";
                std::cout << oss.str() << std::endl;
                throw eckit::UserError(oss.str(), Here());
            }

            const auto lv = level ? *level : *levelist;
            searchTOLSetter->second(g, *typeOfLevel, lv);
        }
        else {
            g.setValue(dm::legacy::TypeOfLevel, *typeOfLevel);
            withFirstOf(valueSetter(g, dm::legacy::Level), lookUp<std::int64_t>(md, dm::legacy::Level),
                        lookUp<std::int64_t>(md, dm::legacy::Levelist));
        }
    }

    const auto productionStatusOfProcessedData
        = lookUp<std::int64_t>(md, dm::legacy::ProductionStatusOfProcessedData)();
    if (productionStatusOfProcessedData) {
        g.setValue(dm::legacy::ProductionStatusOfProcessedData, *productionStatusOfProcessedData);
    }

    ret.paramId = firstOf(
        lookUp<std::int64_t>(md, dm::legacy::ParamId),
        lookUpTranslate<std::int64_t>(md, dm::legacy::Param));  // param might be a string, separated by . for GRIB1.
                                                                // String to std::int64_t convertion should get it right
    if (ret.paramId) {
        g.setValue(dm::legacy::ParamId, *ret.paramId);
    }
    withFirstOf(valueSetter(g, dm::legacy::ClassKey), lookUp<std::string>(md, dm::legacy::ClassKey),
                lookUp<std::string>(md, "marsClass"));
    withFirstOf(valueSetter(g, dm::legacy::Stream), lookUp<std::string>(md, dm::legacy::Stream),
                lookUp<std::string>(md, "marsStream"));
    withFirstOf(valueSetter(g, dm::legacy::Expver), lookUp<std::string>(md, dm::legacy::Expver),
                lookUp<std::string>(md, "experimentVersionNumber"));

    if (auto searchLevTypeWam = md.find(dm::legacy::LevtypeWam); searchLevTypeWam != md.end() && (gribEdition == "2")) {
        g.setValue(dm::legacy::TypeOfFirstFixedSurface, 1);
        g.setMissing(dm::legacy::ScaleFactorOfFirstFixedSurface);
        g.setMissing(dm::legacy::ScaledValueOfFirstFixedSurface);
        g.setMissing(dm::legacy::ScaleFactorOfSecondFixedSurface);
        g.setMissing(dm::legacy::ScaledValueOfSecondFixedSurface);
    }

    if (gribEdition == "2") {
        withFirstOf(valueSetter(g, dm::legacy::SubCentre), lookUp<std::int64_t>(md, dm::legacy::SubCentre));
        withFirstOf(valueSetter(g, dm::legacy::TablesVersion), lookUp<std::int64_t>(md, dm::legacy::TablesVersion));
        withFirstOf(valueSetter(g, dm::legacy::LocalTablesVersion),
                    lookUp<std::int64_t>(md, dm::legacy::LocalTablesVersion));
        withFirstOf(valueSetter(g, dm::legacy::SetLocalDefinition),
                    lookUp<std::int64_t>(md, dm::legacy::SetLocalDefinition));
        withFirstOf(valueSetter(g, dm::legacy::Grib2LocalSectionNumber),
                    lookUp<std::int64_t>(md, dm::legacy::Grib2LocalSectionNumber));

        if (productionStatusOfProcessedData) {
            if (*productionStatusOfProcessedData == 12) {
                const auto dataset = lookUp<std::string>(md, "dataset")();
                if (dataset) {
                    g.setValue("dataset", *dataset);

                    if (*dataset == "climate-dt") {
                        withFirstOf(valueSetter(g, dm::legacy::Activity),
                                    lookUp<std::string>(md, dm::legacy::Activity));
                        withFirstOf(valueSetter(g, dm::legacy::Experiment),
                                    lookUp<std::string>(md, dm::legacy::Experiment));
                        withFirstOf(valueSetter(g, dm::legacy::Generation),
                                    lookUp<std::string>(md, dm::legacy::Generation));
                        withFirstOf(valueSetter(g, dm::legacy::Model), lookUp<std::string>(md, dm::legacy::Model));
                        withFirstOf(valueSetter(g, dm::legacy::Realization),
                                    lookUp<std::string>(md, dm::legacy::Realization));
                        withFirstOf(valueSetter(g, dm::legacy::Resolution),
                                    lookUp<std::string>(md, dm::legacy::Resolution));
                    }
                }
            }

            if (*productionStatusOfProcessedData == 2) {
                const auto data_class = lookUp<std::string>(md, dm::legacy::ClassKey)();
                if (data_class == "ed") {
                    withFirstOf(valueSetter(g, "activity"), lookUp<std::string>(md, "activity"));
                    withFirstOf(valueSetter(g, "experiment"), lookUp<std::string>(md, "experiment"));
                    withFirstOf(valueSetter(g, "realization"), lookUp<std::string>(md, "realization"));
                    withFirstOf(valueSetter(g, "generation"), lookUp<std::string>(md, "generation"));
                    withFirstOf(valueSetter(g, "model"), lookUp<std::string>(md, "model"));
                    withFirstOf(valueSetter(g, "resolution"), lookUp<std::string>(md, "resolution"));
                }
            }
        }
    }


    withFirstOf(valueSetter(g, dm::legacy::GeneratingProcessIdentifier),
                lookUp<std::int64_t>(md, dm::legacy::GeneratingProcessIdentifier));

    withFirstOf(valueSetter(g, "number"), lookUp<std::int64_t>(md, dm::legacy::EnsembleMember),
                lookUp<std::int64_t>(md, dm::legacy::EnsembleMemberKC));
    withFirstOf(valueSetter(g, "numberOfForecastsInEnsemble"), lookUp<std::int64_t>(md, dm::legacy::EnsembleSize),
                lookUp<std::int64_t>(md, dm::legacy::EnsembleSizeKC));

    withFirstOf(valueSetter(g, dm::legacy::MethodNumber), lookUp<std::int64_t>(md, dm::legacy::MethodNumber),
                lookUp<std::int64_t>(md, dm::legacy::MethodNumberKC));
    withFirstOf(valueSetter(g, dm::legacy::SystemNumber), lookUp<std::int64_t>(md, dm::legacy::SystemNumber),
                lookUp<std::int64_t>(md, dm::legacy::SystemNumberKC));


    withFirstOf(valueSetter(g, dm::legacy::Expver), lookUp<std::string>(md, dm::legacy::Expver),
                lookUp<std::string>(md, dm::legacy::ExperimentVersionNumber));
    withFirstOf(
        valueSetter(g, dm::legacy::PerturbationNumber), lookUp<std::int64_t>(md, dm::legacy::PerturbationNumber),
        lookUp<std::int64_t>(md, dm::legacy::EnsembleMember), lookUp<std::int64_t>(md, dm::legacy::EnsembleMemberKC));
    withFirstOf(valueSetter(g, dm::legacy::NumberOfForecastsInEnsemble),
                lookUp<std::int64_t>(md, dm::legacy::NumberOfForecastsInEnsemble),
                lookUp<std::int64_t>(md, dm::legacy::EnsembleSize), lookUp<std::int64_t>(md, dm::legacy::EnsembleSize),
                lookUp<std::int64_t>(md, dm::legacy::EnsembleSizeKC));
    withFirstOf(valueSetter(g, dm::legacy::OffsetToEndOf4DvarWindow),
                lookUp<std::int64_t>(md, dm::legacy::OffsetToEndOf4DvarWindow),
                lookUp<std::int64_t>(md, dm::legacy::Anoffset));
    withFirstOf(valueSetter(g, dm::legacy::LengthOf4DvarWindow),
                lookUp<std::int64_t>(md, dm::legacy::LengthOf4DvarWindow),
                lookUp<std::int64_t>(md, dm::legacy::Anlength));

    // Metadata for ensemble forecast
    withFirstOf(valueSetter(g, "oceanAtmosphereCoupling"), lookUp<std::int64_t>(md, "oceanAtmosphereCoupling"));
    withFirstOf(valueSetter(g, "legBaseDate"), lookUp<std::int64_t>(md, "legBaseDate"));
    withFirstOf(valueSetter(g, "legBaseTime"), lookUp<std::int64_t>(md, "legBaseTime"));
    withFirstOf(valueSetter(g, "legNumber"), lookUp<std::int64_t>(md, "legNumber"));
    withFirstOf(valueSetter(g, "referenceDate"), lookUp<std::int64_t>(md, "referenceDate"));
    withFirstOf(valueSetter(g, "climateDateFrom"), lookUp<std::int64_t>(md, "climateDateFrom"));
    withFirstOf(valueSetter(g, "climateDateTo"), lookUp<std::int64_t>(md, "climateDateTo"));

    withFirstOf(valueSetter(g, dm::legacy::ComponentIndex), lookUp<std::int64_t>(md, dm::legacy::ComponentIndex));
    withFirstOf(valueSetter(g, dm::legacy::NumberOfComponents),
                lookUp<std::int64_t>(md, dm::legacy::NumberOfComponents));
    withFirstOf(valueSetter(g, dm::legacy::ModelErrorType), lookUp<std::int64_t>(md, dm::legacy::ModelErrorType));
    withFirstOf(valueSetter(g, dm::legacy::IterationNumber), lookUp<std::int64_t>(md, dm::legacy::IterationNumber));
    withFirstOf(valueSetter(g, dm::legacy::TotalNumberOfIterations),
                lookUp<std::int64_t>(md, dm::legacy::TotalNumberOfIterations));

    ret.type = firstOf(lookUp<std::string>(md, dm::legacy::Type), lookUp<std::string>(md, dm::legacy::MarsType));
    if (ret.type) {
        g.setValue(dm::legacy::Type, *ret.type);
    }

    // Additional parameters passed through for spherical harmonics
    if (gridType) {
        if (*gridType == "sh") {
            withFirstOf(valueSetter(g, dm::legacy::ComplexPacking),
                        lookUp<std::int64_t>(md, dm::legacy::ComplexPacking));
            withFirstOf(valueSetter(g, dm::legacy::PentagonalResolutionParameterJ),
                        lookUp<std::int64_t>(md, dm::legacy::PentagonalResolutionParameterJ),
                        lookUp<std::int64_t>(md, dm::legacy::J));
            withFirstOf(valueSetter(g, dm::legacy::PentagonalResolutionParameterK),
                        lookUp<std::int64_t>(md, dm::legacy::PentagonalResolutionParameterK),
                        lookUp<std::int64_t>(md, dm::legacy::K));
            withFirstOf(valueSetter(g, dm::legacy::PentagonalResolutionParameterM),
                        lookUp<std::int64_t>(md, dm::legacy::PentagonalResolutionParameterM),
                        lookUp<std::int64_t>(md, dm::legacy::M));

            withFirstOf(valueSetter(g, dm::legacy::SubSetJ), lookUp<std::int64_t>(md, dm::legacy::SubSetJ),
                        lookUp<std::int64_t>(md, dm::legacy::Js));
            withFirstOf(valueSetter(g, dm::legacy::SubSetK), lookUp<std::int64_t>(md, dm::legacy::SubSetK),
                        lookUp<std::int64_t>(md, dm::legacy::Ks));
            withFirstOf(valueSetter(g, dm::legacy::SubSetM), lookUp<std::int64_t>(md, dm::legacy::SubSetM),
                        lookUp<std::int64_t>(md, dm::legacy::Ms));
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
            std::optional<double> latitudeOfFirstGridPointInDegrees;
            std::optional<double> longitudeOfFirstGridPointInDegrees;
            std::optional<double> latitudeOfLastGridPointInDegrees;
            std::optional<double> longitudeOfLastGridPointInDegrees;
            std::optional<double> iDirectionIncrementInDegrees;
            std::optional<double> jDirectionIncrementInDegrees;
            if ((ni = lookUp<std::int64_t>(md, dm::legacy::Ni)()) && (nj = lookUp<std::int64_t>(md, dm::legacy::Nj)())
                && (north = lookUp<double>(md, dm::legacy::North)())
                && (south = lookUp<double>(md, dm::legacy::South)()) && (west = lookUp<double>(md, dm::legacy::West)())
                && (east = lookUp<double>(md, dm::legacy::East)())
                && (westEastInc = lookUp<double>(md, dm::legacy::WestEastIncrement)())
                && (southNorthInc = lookUp<double>(md, dm::legacy::SouthNorthIncrement)())) {
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
            else if ((ni = lookUp<std::int64_t>(md, dm::legacy::Ni)())
                     && (nj = lookUp<std::int64_t>(md, dm::legacy::Nj)())
                     && (latitudeOfFirstGridPointInDegrees
                         = lookUp<double>(md, dm::legacy::LatitudeOfFirstGridPointInDegrees)())
                     && (latitudeOfLastGridPointInDegrees
                         = lookUp<double>(md, dm::legacy::LatitudeOfLastGridPointInDegrees)())
                     && (longitudeOfFirstGridPointInDegrees
                         = lookUp<double>(md, dm::legacy::LongitudeOfFirstGridPointInDegrees)())
                     && (longitudeOfLastGridPointInDegrees
                         = lookUp<double>(md, dm::legacy::LongitudeOfLastGridPointInDegrees)())
                     && (iDirectionIncrementInDegrees = lookUp<double>(md, dm::legacy::IDirectionIncrementInDegrees)())
                     && (jDirectionIncrementInDegrees
                         = lookUp<double>(md, dm::legacy::JDirectionIncrementInDegrees)())) {
                g.setValue("Ni", *ni);
                g.setValue("Nj", *nj);
                g.setValue(dm::legacy::LatitudeOfFirstGridPointInDegrees, *latitudeOfFirstGridPointInDegrees);
                g.setValue(dm::legacy::LongitudeOfFirstGridPointInDegrees, *longitudeOfFirstGridPointInDegrees);
                g.setValue(dm::legacy::LatitudeOfLastGridPointInDegrees, *latitudeOfLastGridPointInDegrees);
                g.setValue(dm::legacy::LongitudeOfLastGridPointInDegrees, *longitudeOfLastGridPointInDegrees);
                g.setValue(dm::legacy::IDirectionIncrementInDegrees, *iDirectionIncrementInDegrees);
                g.setValue(dm::legacy::JDirectionIncrementInDegrees, *jDirectionIncrementInDegrees);
            }
        }
        else if (eckit::StringTools::lower(*gridType) == "healpix") {
            withFirstOf(valueSetter(g, "Nside"), lookUp<std::int64_t>(md, dm::legacy::Nside));
            double logp = 45.0;
            // Note: Pedro told to use always this to avoid problems with milli and micro degrees
            g.setValue("longitudeOfFirstGridPointInDegrees", 45.0);
            withFirstOf(valueSetter(g, "orderingConvention"), lookUp<std::string>(md, dm::legacy::OrderingConvention));
        }
    }

    return ret;
}

void applyOverwrites(GribEncoder& g, const message::Metadata& md) {
    if (auto searchOverwrites = md.find("encoder-overwrites"); searchOverwrites != md.end()) {
        // TODO Refactor with visitor
        for (const auto& kv : searchOverwrites->second.get<message::BaseMetadata>()) {
            if (g.hasKey(kv.first.value().c_str())) {
                kv.second.visit(eckit::Overloaded{
                    [](const auto& v) -> util::IfTypeOf<decltype(v), MetadataTypes::AllNested> {},
                    [&g, &kv](const auto& vec) -> util::IfTypeOf<decltype(vec), MetadataTypes::Lists> {
                        if constexpr (std::is_same_v<std::decay_t<decltype(vec)>, std::vector<bool>>) {
                            throw eckit::UserError(
                                "Writing vector<bool> to an eccodes handle is currently not supported", Here());
                        }
                        else {
                            g.setValue(kv.first, vec);
                        }
                    },
                    [&g, &kv](const auto& v) -> util::IfTypeOf<decltype(v), MetadataTypes::NonNullScalars> {
                        g.setValue(kv.first, v);
                    },
                    [&g, &kv](const auto& v) -> util::IfTypeOf<decltype(v), MetadataTypes::Nulls> {
                        g.setValue(kv.first, 0);
                    }});
            }
        }
    }
}

void applyOverwrites(GribEncoder& g, const CodesOverwrites& overwrites) {
    for (const auto& kv : overwrites) {
        std::visit([&](const auto& v) { g.setValue(kv.first, v); }, kv.second);
    }
}


void setEncodingSpecificFields(GribEncoder& g, const message::Metadata& md) {
    // TODO globalSize is expected to be set in md directly. nmuberOf* should be
    // readonly anyway... test removal..

    withFirstOf(valueSetter(g, dm::legacy::MissingValue), lookUp<double>(md, dm::legacy::MissingValue));
    withFirstOf(valueSetter(g, dm::legacy::BitmapPresent), lookUp<bool>(md, dm::legacy::BitmapPresent));
    withFirstOf(valueSetter(g, dm::legacy::BitsPerValue), lookUp<std::int64_t>(md, dm::legacy::BitsPerValue));
}

std::string getTimeReference(GribEncoder& g, const message::Metadata& md, const QueriedMarsKeys& queriedMarsFields,
                             const std::string& gribEdition, bool isTimeRange,
                             const std::optional<std::int64_t> significanceOfReferenceTime) {
    if (auto optTimeRef = lookUp<std::string>(md, "timeReference")(); optTimeRef) {
        return *optTimeRef;
    }

    // TODO: this will not hold in the future - maybe the new category "processType" can be used to check if it's a
    // forecast
    // Handling of significanceOfReferenceTime is hacked in for now....
    bool isReferingToStart = false;
    if (queriedMarsFields.type) {
        if (*queriedMarsFields.type == "fc") {
            if ((gribEdition == "2") && significanceOfReferenceTime && (*significanceOfReferenceTime == 2)) {
                isReferingToStart = false;
            }
            else {
                isReferingToStart = true;
            }
        }
        else if (types_with_time_reference_offset.find(*queriedMarsFields.type)
                 != types_with_time_reference_offset.end()) {
            isReferingToStart = true;
        }
    }

    return isReferingToStart ? "start" : (isTimeRange ? "previous" : "current");
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
            const auto& overwrites = md.get<message::BaseMetadata>("encoder-overwrites");
            significanceOfReferenceTime = lookUp<std::int64_t>(overwrites, "significanceOfReferenceTime")();
        }
    }
    if ((gribEdition == "2") && significanceOfReferenceTime) {
        g.setValue("significanceOfReferenceTime", *significanceOfReferenceTime);
    }

    tryMapStepToTimeAndCheckTime(md);

    std::string timeRef
        = getTimeReference(g, md, queriedMarsFields, gribEdition, isTimeRange, significanceOfReferenceTime);

    auto refDateTimeTup = getReferenceDateTime(timeRef, md);
    auto refDateTime = util::wrapDateTime(
        {util::toDateInts(std::get<0>(refDateTimeTup)), util::toTimeInts(std::get<1>(refDateTimeTup))});
    g.trySetValue("year", refDateTime.date.year);
    g.trySetValue("month", refDateTime.date.month);
    g.trySetValue("day", refDateTime.date.day);

    g.trySetValue("hour", refDateTime.time.hour);
    g.trySetValue("minute", refDateTime.time.minute);
    g.trySetValue("second", refDateTime.time.second);

    auto currentDateTime = util::wrapDateTime({util::toDateInts(md.get<std::int64_t>(dm::legacy::CurrentDate)),
                                               util::toTimeInts(md.get<std::int64_t>(dm::legacy::CurrentTime))});

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
        auto previousDateTime = util::wrapDateTime({util::toDateInts(md.get<std::int64_t>(dm::legacy::PreviousDate)),
                                                    util::toTimeInts(md.get<std::int64_t>(dm::legacy::PreviousTime))});

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

            // Is this really needed on top of setting stepUnits?
            g.setValue("indicatorOfUnitOfTimeRange", timeUnitCodes(util::TimeUnit::Hour));
            g.setValue("forecastTime", 0l);

            // Set endStep to please MARS
            g.setValue("stepUnits", timeUnitCodes(util::TimeUnit::Hour));
            g.setValue("endStep", util::dateTimeDiffInSeconds(currentDateTime.date, currentDateTime.time,
                                                              previousDateTime.date, previousDateTime.time)
                                      / 3600);
        }

        if (operation && (*operation != "instant")) {
            const auto searchStat = type_of_statistical_processing.find(*operation);
            if (searchStat == std::end(type_of_statistical_processing)) {
                std::ostringstream oss;
                oss << "setDateAndStatisticalFields - Cannot map value \"" << *operation
                    << "\"for key \"operation\" (statistical output) to a valid grib2 type of statistical processing.";
                throw eckit::UserError(oss.str(), Here());
            }
            g.setValue("typeOfStatisticalProcessing", searchStat->second);
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
        g.setValue("typeOfTimeIncrement", (timeRef == "start" ? 2 : 255));

        if (const auto timeIncrement = md.getOpt<std::int64_t>(dm::legacy::TimeIncrement); timeIncrement) {
            if (*timeIncrement != 0) {
                withFirstOf(valueSetter(g, "indicatorOfUnitForTimeIncrement"),
                            lookUp<std::int64_t>(md, dm::legacy::IndicatorOfUnitForTimeIncrement));
                g.setValue("timeIncrement", *timeIncrement);
            }
            else {
                g.setValue("indicatorOfUnitForTimeIncrement", 255);
                g.setValue("timeIncrement", 0);
            }
        }
        else if (const auto sampleIntervalInSeconds = md.getOpt<std::int64_t>(dm::legacy::SampleIntervalInSeconds);
                 sampleIntervalInSeconds) {
            g.setValue("indicatorOfUnitForTimeIncrement", timeUnitCodes(util::TimeUnit::Second));
            g.setValue("timeIncrement", *sampleIntervalInSeconds);
        }
        else {
            g.setValue("indicatorOfUnitForTimeIncrement", timeUnitCodes(util::TimeUnit::Second));
            withFirstOf(valueSetter(g, "timeIncrement"),
                        lookUp<std::int64_t>(md, dm::legacy::TimeStep)());  // Nemo is currently sending timeStep
        }
    }
    else {
        // Do nothing special for GRIB1 -- Time-range encoding not supported?
    }


    auto dateOfAnalysis = firstOf(lookUp<std::int64_t>(md, dm::legacy::DateOfAnalysis));
    auto timeOfAnalysis = firstOf(lookUp<std::int64_t>(md, dm::legacy::TimeOfAnalysis)).value_or(0);
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

void GribEncoder::setFieldMetadata(message::Metadata& md) {
    if (isOcean(md)) {
        setOceanMetadata(md);
    }
    else {
        auto queriedMarsFields = setMarsKeys(*this, md);
        setEncodingSpecificFields(*this, md);
        setDateAndStatisticalFields(*this, md, queriedMarsFields);
    }
}

namespace {}

void GribEncoder::setOceanMetadata(message::Metadata& md) {
    auto queriedMarsFields = setMarsKeys(*this, md);
    if (queriedMarsFields.type) {
        setValue(dm::legacy::TypeOfGeneratingProcess, type_of_generating_process.at(*queriedMarsFields.type));
    }

    setDateAndStatisticalFields(*this, md, queriedMarsFields);
    setEncodingSpecificFields(*this, md);

    // Setting parameter ID
    auto paramInt = md.get<std::int64_t>(dm::legacy::ParamId);
    if (paramInt / 1000 == 212) {
        // HACK! Support experimental averages.
        setValue(dm::legacy::ParamId, paramInt + 4000);
    }
    else {
        const auto operation = md.getOpt<std::string>(dm::legacy::Operation);
        const auto paramIdIncrement = operation ? ops_to_code.at(*operation) : 0;
        setValue(dm::legacy::ParamId, paramInt + paramIdIncrement);
    }

    const auto& typeOfLevel = md.get<std::string>(dm::legacy::TypeOfLevel);
    setValue(dm::legacy::TypeOfLevel, typeOfLevel);
    if (typeOfLevel == "oceanModelLayer") {
        auto level = md.get<std::int64_t>(dm::legacy::Level);
        ASSERT(level > 0);
        setValue("scaleFactorOfFirstFixedSurface", 0l);
        setValue("scaledValueOfFirstFixedSurface", level - 1);
        setValue("scaleFactorOfSecondFixedSurface", 0l);
        setValue("scaledValueOfSecondFixedSurface", level);
        setValue("scaleFactorOfFirstFixedSurface", 0l);
        setValue("scaleFactorOfSecondFixedSurface", 0l);
    }
    if (typeOfLevel == "oceanModel") {
        auto level = md.get<std::int64_t>("level");
        ASSERT(level > 0);
        setValue("scaledValueOfFirstFixedSurface", level);
        setValue("scaleFactorOfFirstFixedSurface", 0l);
    }


    std::string gridType;
    const auto searchGridType = md.find(dm::legacy::GridType);
    if (searchGridType != md.end() && searchGridType->second.get<std::string>() == "unstructured_grid") {
        if (auto searchGridType = md.find("unstructuredGridType"); searchGridType != md.end()) {
            setValue(dm::legacy::UnstructuredGridType, searchGridType->second.template get<std::string>());
        }
        else {
            setValue(dm::legacy::UnstructuredGridType, getUnstructuredGridType(config_));
        }

        if (auto searchGridSubtype = md.find(dm::legacy::UnstructuredGridSubtype); searchGridSubtype != md.end()) {
            setValue("unstructuredGridSubtype", searchGridSubtype->second.template get<std::string>());
        }

        if (auto searchGridUUID = md.find("uuidOfHGrid"); searchGridUUID != md.end()) {
            setValue("uuidOfHGrid", searchGridUUID->second.template get<std::string>());
        }
        else {
            eckit::Log::warning() << "Ocean grid UUID not available during encoding!" << std::endl;
        }
    }
    else if (eckit::StringTools::lower(gridType) == "healpix") {
        long Nside = md.get<std::int64_t>("Nside");
        setValue("Nside", Nside);
        double logp = 45.0;
        // Note: Pedro told to use always this to avoid problems with milli and micro degrees
        setValue("longitudeOfFirstGridPointInDegrees", logp);
        setValue("orderingConvention", md.get<std::string>("orderingConvention"));
    }
}

void GribEncoder::setOceanCoordMetadata(message::Metadata& md) {
    setMarsKeys(*this, md);

    setValue(dm::legacy::Date, md.get<std::int64_t>(dm::legacy::StartDate));

    // Setting parameter ID
    auto paramInt = md.get<std::int64_t>(dm::legacy::ParamId);
    setValue(dm::legacy::ParamId, paramInt);

    setValue(dm::legacy::TypeOfLevel, md.get<std::string>(dm::legacy::TypeOfLevel));

    // Set ocean grid information
    setValue(dm::legacy::UnstructuredGridType, getUnstructuredGridType(config_));

    const auto& gridSubtype
        = md.get<std::string>("gridSubtype");  // TO BE REMOVED IN THE FUTURE - should be named unstructuredGridType
    setValue(dm::legacy::UnstructuredGridSubtype, gridSubtype.substr(0, 1));

    const auto& gridUID = md.get<std::string>(dm::legacy::UuidOfHGrid);
    setValue(dm::legacy::UuidOfHGrid, gridUID);

    // Set encoding for missing value support
    setValue(dm::legacy::BitmapPresent, false);
    setValue(dm::legacy::BitsPerValue, md.get<std::int64_t>(dm::legacy::BitsPerValue));
}


void GribEncoder::initEncoder() {
    encoder_ = template_->clone();
};

bool GribEncoder::hasKey(const char* key) {
    return encoder_->has(key);
};

void GribEncoder::setMissing(const std::string& key) {
    encoder_->setMissing(key);
}

message::Message GribEncoder::encodeOceanCoordinates(message::Message&& msg,
                                                     const message::Metadata& additionalMetadata) {
    initEncoder();
    msg.header().acquireMetadata();

    auto& metadata = msg.modifyMetadata();
    metadata.updateOverwrite(additionalMetadata);
    applyOverwrites(*this, metadata);
    setOceanCoordMetadata(metadata);

    return dispatchPrecisionTag(msg.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        return setFieldValues<Precision>(std::move(msg));
    });
}

message::Message GribEncoder::encodeField(message::Message&& msg, const CodesOverwrites& overwrites,
                                          const message::Metadata& additionalMetadata) {
    initEncoder();
    msg.header().acquireMetadata();

    applyOverwrites(*this, overwrites);
    auto& metadata = msg.modifyMetadata();
    metadata.updateOverwrite(additionalMetadata);
    applyOverwrites(*this, metadata);

    setFieldMetadata(metadata);
    return dispatchPrecisionTag(msg.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        return setFieldValues<Precision>(std::move(msg));
    });
}


template <typename T>
message::Message GribEncoder::setFieldValues(message::Message&& msg) {
    auto beg = reinterpret_cast<const T*>(msg.payload().data());

    this->setDataValues(beg, msg.globalSize());

    msg.header().acquireMetadata();
    const auto& metadata = msg.metadata();
    auto offsetByValue = metadata.getOpt<double>("offsetValuesBy");
    if (offsetByValue) {
        setValue("offsetValuesBy", *offsetByValue);
    }

    eckit::Buffer buf{this->encoder_->messageSize()};
    encoder_->copyInto(reinterpret_cast<uint8_t*>(buf.data()), buf.size());

    return Message{Message::Header{Message::Tag::Field, Peer{msg.source().group()}, Peer{msg.destination()}},
                   std::move(buf)};
}


void GribEncoder::print(std::ostream& os) const {
    os << "GribEncoder(config=" << config_ << ")";
};

}  // namespace multio::action::encode
