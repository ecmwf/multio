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
#include <unordered_map>
#include <sstream>

#include "fakeDoubleLoop.h"

#include "multio/LibMultio.h"

#include "eckit/config/LocalConfiguration.h"
#include "eckit/config/YAMLConfiguration.h"
#include "eckit/filesystem/PathName.h"

namespace multio::action::encode_mtg2::fake_double_loop {

namespace detail {

using Param = std::int64_t;

enum class TypeOfStatisticalProcessing : std::int64_t {
    Average           = 0,
    Accumulation      = 1,
    Maximum           = 2,
    Minimum           = 3,
    Difference        = 4,
    StandardDeviation = 6,
    InverseDifference = 8
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
            eckit::YAMLConfiguration{
                eckit::PathName{
                    multio::LibMultio::instance().libraryHome()
                    + "/share/multio/mappings/statistics_operation_mappings.yaml"
                }
            }
        };

        Mapping operationMappings;

        for (const auto& mapping : mappingConf.getSubConfigurations()) {
            const auto param = mapping.getInt64("param");
            const auto typeOfStatisticalProcessing =
                mapping.getInt64("typeOfStatisticalProcessing");

            operationMappings.emplace(
                param,
                typeOfStatisticalProcessingFromInt(typeOfStatisticalProcessing)
            );
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
    if (klass == "e6" && (stream == "sttd" || stream == "stte" ) ){
        return true;
    }

    // Rule valid for SEAS6
    if ( ( klass == "od" || klass == "rd" || klass == "c3" ) && (stream == "sfmd" || stream == "shmd" ) ){
        return true;
    }

    // Other rules
    if ( ( klass == "gh" || klass == "eh") && ( stream == "msmm" || stream == "rfsd" ) ){
        return true;
    }

    return false;

}

std::optional<std::string> operationCodeFromParam(std::int64_t param){

    // This is the full 
    const std::optional<TypeOfStatisticalProcessing> operation =
        LocalStatisticsOperationMapping::instance().getOperation(param);

    if (operation && isValidStattypeOperation(*operation)) {
        return stattypeOperationCode(*operation);
    }  else {
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
        return *operationCode + *periodCode;
    } else {
        std::ostringstream os;
        os << "Cannot reconstruct statType for single loop statistics record ";
        os << "with param: " << param << " and timespan (hours): " << timespan;
        throw eckit::SeriousBug(os.str(), Here());
    }

}

} // namespace detail

void fakeDoubleLoop( dm::FullMarsRecord& marsRec) {
    
    if ( detail::isSingleLoopStatistics( marsRec ) ) {
        if ( detail::requiresFakeDoubleLoopRepresentation( marsRec ) ) {
            std::string stattype = detail::reconstructStatType(marsRec);
            marsRec.stattype.set(dm::TypeParser<dm::StatType>::parse(stattype));
            marsRec.timespan.set(dm::TypeParser<dm::TimeSpan>::parse("none"));
        }
    }

}

} // namespace multio::action::encode_mtg2::fake_double_loop