/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <optional>
#include <sstream>
#include <unordered_map>

#include "fakeDoubleLoop.h"

#include "multio/LibMultio.h"

#include "eckit/config/LocalConfiguration.h"
#include "eckit/config/YAMLConfiguration.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/types/DateTime.h"

namespace multio::action::encode_mtg2::fake_double_loop {

namespace detail {

using Param = std::int64_t;

enum class TypeOfStatisticalProcessing : std::int64_t
{
    Average = 0,
    Accumulation = 1,
    Maximum = 2,
    Minimum = 3,
    Difference = 4,
    StandardDeviation = 6,
    InverseDifference = 8,
    Severity = 100,
    Mode = 101
};


TypeOfStatisticalProcessing typeOfStatisticalProcessingFromInt(std::int64_t value) {
    switch (value) {
        case 0:
            return TypeOfStatisticalProcessing::Average;
        case 1:
            return TypeOfStatisticalProcessing::Accumulation;
        case 2:
            return TypeOfStatisticalProcessing::Maximum;
        case 3:
            return TypeOfStatisticalProcessing::Minimum;
        case 4:
            return TypeOfStatisticalProcessing::Difference;
        case 6:
            return TypeOfStatisticalProcessing::StandardDeviation;
        case 8:
            return TypeOfStatisticalProcessing::InverseDifference;
        case 100:
            return TypeOfStatisticalProcessing::Severity;
        case 101:
            return TypeOfStatisticalProcessing::Mode;
        default: {
            std::ostringstream os;
            os << "Unknown typeOfStatisticalProcessing value: " << value;
            throw eckit::SeriousBug(os.str(), Here());
        }
    }
}


bool isValidStattypeOperation(TypeOfStatisticalProcessing operation) {
    switch (operation) {
        case TypeOfStatisticalProcessing::Average:
        case TypeOfStatisticalProcessing::StandardDeviation:
        case TypeOfStatisticalProcessing::Minimum:
        case TypeOfStatisticalProcessing::Maximum:
            return true;

        case TypeOfStatisticalProcessing::Accumulation:
        case TypeOfStatisticalProcessing::Difference:
        case TypeOfStatisticalProcessing::InverseDifference:
        case TypeOfStatisticalProcessing::Severity:
        case TypeOfStatisticalProcessing::Mode:
            return false;
    }

    return false;
}


std::string stattypeOperationCode(TypeOfStatisticalProcessing operation) {
    switch (operation) {
        case TypeOfStatisticalProcessing::Average:
            return "av";
        case TypeOfStatisticalProcessing::StandardDeviation:
            return "sd";
        case TypeOfStatisticalProcessing::Minimum:
            return "mn";
        case TypeOfStatisticalProcessing::Maximum:
            return "mx";

        case TypeOfStatisticalProcessing::Accumulation:
        case TypeOfStatisticalProcessing::Difference:
        case TypeOfStatisticalProcessing::InverseDifference:
        case TypeOfStatisticalProcessing::Severity:
        case TypeOfStatisticalProcessing::Mode:
            break;
    }

    std::ostringstream os;
    os << "TypeOfStatisticalProcessing cannot be converted to a valid stattype operation code";
    throw eckit::SeriousBug(os.str(), Here());
}


class LocalStatisticsOperationMapping {
public:
    using Mapping = std::unordered_map<Param, TypeOfStatisticalProcessing>;

    static const LocalStatisticsOperationMapping& instance() {
        static const LocalStatisticsOperationMapping mapping = make();
        return mapping;
    }

    std::optional<TypeOfStatisticalProcessing> getOperation(Param param) const {
        const auto it = operationMappings_.find(param);

        if (it == operationMappings_.end()) {
            return std::nullopt;
        }

        return it->second;
    }

private:
    explicit LocalStatisticsOperationMapping(Mapping operationMappings) :
        operationMappings_{std::move(operationMappings)} {}

    static LocalStatisticsOperationMapping make() {
        eckit::LocalConfiguration mappingConf{
            eckit::YAMLConfiguration{eckit::PathName{multio::LibMultio::instance().libraryHome()
                                                     + "/share/multio/mappings/statistics_operation_mappings.yaml"}}};

        Mapping operationMappings;

        for (const auto& mapping : mappingConf.getSubConfigurations()) {
            const auto param = mapping.getInt64("param");
            const auto typeOfStatisticalProcessing = mapping.getInt64("typeOfStatisticalProcessing");
            //            std::cout << "MIVAL: Mapping param: " << param << " to typeOfStatisticalProcessing: "
            //                      << typeOfStatisticalProcessing << std::endl;
            operationMappings.emplace(param, typeOfStatisticalProcessingFromInt(typeOfStatisticalProcessing));
        }

        return LocalStatisticsOperationMapping{std::move(operationMappings)};
    }

    Mapping operationMappings_;
};


bool isSingleLoopStatistics(const dm::FullMarsRecord& marsRec) {
    // A single loop statistics record is defined as having a timespan but no statType
    // @todo A more refined check can be done by checking the value of the param key and lokup
    // the param in a map of statistics parameters (e.g. 228128 for ECMWF) - but this is sufficient
    // for now to trigger the fake double loop representation when needed
    return marsRec.timespan.isSet() && !marsRec.stattype.isSet();
}

bool requiresFakeDoubleLoopRepresentation(const dm::FullMarsRecord& marsRec) {
    // FakeDoubleLoop is a "hacked" representation of single loop statistics in a
    // double loop fashion. Basically there was a requirement to add statType also
    // for single loop statistics to simplify the requests
    std::string klass = marsRec.klass.get();
    std::string stream = marsRec.stream.get();

    // Rule valid for ERA6 products
    if (klass == "e6" && (stream == "sttd" || stream == "stte")) {
        return true;
    }

    // Rule valid for SEAS6
    if ((klass == "od" || klass == "rd" || klass == "c3") && (stream == "sfmd" || stream == "shmd")) {
        return true;
    }

    // Other rules
    if ((klass == "gh" || klass == "eh") && (stream == "msmm" || stream == "rfsd")) {
        return true;
    }

    return false;
}

bool isSeasonal(const dm::FullMarsRecord& marsRec) {
    std::string klass = marsRec.klass.get();
    std::string stream = marsRec.stream.get();

    return (klass == "od" || klass == "rd" || klass == "c3") && (stream == "sfmd" || stream == "shmd");
}

std::optional<std::string> operationCodeFromParam(std::int64_t param) {

    // This is the full
    const std::optional<TypeOfStatisticalProcessing> operation
        = LocalStatisticsOperationMapping::instance().getOperation(param);

    if (operation && isValidStattypeOperation(*operation)) {
        return stattypeOperationCode(*operation);
    }
    else {
        return std::nullopt;
    }
}

std::optional<std::string> periodCodeFromTimespanHours(std::int64_t timespanHours) {
    // @todo This is a very simplified logic to reconstruct the period code from the timespan hours,
    // it should be refined by looking at the actual value of the timespan and other metadata keys
    // (e.g. step, time, date) to reconstruct the period in a more accurate way.
    // Check calendar here may be too expensive.
    switch (timespanHours) {
        case 24:
            return std::string{"da"};

        case 672:  // 28 * 24
        case 696:  // 29 * 24
        case 720:  // 30 * 24
        case 744:  // 31 * 24
            return std::string{"mo"};

        default:
            return std::nullopt;
    }
}

std::string reconstructStatType(const dm::FullMarsRecord& marsRec) {

    std::int64_t param = marsRec.param.get().id();
    std::int64_t timespan = marsRec.timespan.get().toHours();

    // Get operation code from param
    const std::optional<std::string> operationCode = operationCodeFromParam(param);

    // Get period
    const std::optional<std::string> periodCode = periodCodeFromTimespanHours(timespan);

    // Create statType by concatenating operation code and period code
    if (operationCode && periodCode) {
        return *periodCode + *operationCode;
    }
    else {
        std::ostringstream os;
        os << "Cannot reconstruct statType for single loop statistics record ";
        os << "with param: " << param << " and timespan (hours): " << timespan;
        throw eckit::SeriousBug(os.str(), Here());
    }
}

long computeFcmonth(const dm::FullMarsRecord& marsRec) {
    if (!marsRec.step.isSet()) {
        throw eckit::SeriousBug("Cannot compute fcmonth for seasonal record without step", Here());
    }

    const eckit::Date epochDate{marsRec.date.get()};
    const long epochTime = marsRec.time.get();
    const auto epochHour = epochTime / 10000;
    const auto epochMinute = (epochTime % 10000) / 100;
    const eckit::DateTime epochDateTime{epochDate, eckit::Time{epochHour, epochMinute, 0}};
    const eckit::DateTime currentDateTime
        = epochDateTime + static_cast<eckit::Second>(marsRec.step.get().toHours() * 3600);

    const auto isBeginningOfMonth = [](const eckit::DateTime& dt) {
        return dt.date().day() == 1 && dt.time().hours() == 0 && dt.time().minutes() == 0 && dt.time().seconds() == 0;
    };

    if (!isBeginningOfMonth(epochDateTime)) {
        std::ostringstream os;
        os << "Cannot compute fcmonth: epochDateTime is not at the beginning of a month: " << epochDateTime;
        throw eckit::SeriousBug(os.str(), Here());
    }

    if (!isBeginningOfMonth(currentDateTime)) {
        std::ostringstream os;
        os << "Cannot compute fcmonth: currentDateTime is not at the beginning of a month: " << currentDateTime;
        throw eckit::SeriousBug(os.str(), Here());
    }

    const long fcmonth = static_cast<long>((currentDateTime.date().year() - epochDateTime.date().year()) * 12
                                           + (currentDateTime.date().month() - epochDateTime.date().month()));

    if (fcmonth < 0) {
        std::ostringstream os;
        os << "Cannot compute fcmonth: currentDateTime precedes epochDateTime: " << currentDateTime << " < "
           << epochDateTime;
        throw eckit::SeriousBug(os.str(), Here());
    }

    return fcmonth;
}

} // namespace detail

void fakeDoubleLoop(dm::FullMarsRecord& marsRec) {

    if (detail::isSingleLoopStatistics(marsRec)) {
        if (detail::requiresFakeDoubleLoopRepresentation(marsRec)) {
            std::string stattype = detail::reconstructStatType(marsRec);
            marsRec.stattype.set(dm::TypeParser<dm::StatType>::parse(stattype));
            marsRec.timespan.set(dm::TypeParser<dm::TimeSpan>::parse("none"));
        }
    }
    if (detail::isSeasonal(marsRec)) {
        const long fcmonth = detail::computeFcmonth(marsRec);
        marsRec.fcmonth.set(fcmonth);
        marsRec.step.unset();
    }
}

}  // namespace multio::action::encode_mtg2::fake_double_loop
