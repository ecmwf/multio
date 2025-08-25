/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "StatType.h"

#include "multio/datamod/core/DataModellingException.h"


namespace multio::datamod {

StatType::StatType(SingleStatType first, std::optional<SingleStatType> second) :
    firstLevel_(first), secondLevel_(second) {
    // For multiple levels, the first level must be larger (i.e. moav_damn -> first is month, second is day)
    if (secondLevel_ && !(firstLevel_.duration > secondLevel_->duration)) {
        throw DataModellingException(
            std::string("StatType: Invalid level combination: ") + std::string(DumpType<StatType>::dump(*this)),
            Here());
    }
}

SingleStatType StatType::firstLevel() const {
    return firstLevel_;
};
std::optional<SingleStatType> StatType::secondLevel() const {
    return secondLevel_;
}

std::size_t StatType::levels() const {
    return (secondLevel_ ? 2 : 1);
}


bool operator>(StatTypeDuration lhs, StatTypeDuration rhs) noexcept {
    return std::size_t(lhs) > std::size_t(rhs);
}
bool operator>=(StatTypeDuration lhs, StatTypeDuration rhs) noexcept {
    return std::size_t(lhs) >= std::size_t(rhs);
}
bool operator<(StatTypeDuration lhs, StatTypeDuration rhs) noexcept {
    return std::size_t(lhs) < std::size_t(rhs);
}
bool operator<=(StatTypeDuration lhs, StatTypeDuration rhs) noexcept {
    return std::size_t(lhs) <= std::size_t(rhs);
}

std::string DumpType<StatTypeDuration>::dump(StatTypeDuration v) {
    switch (v) {
        case StatTypeDuration::Day:
            return "da";
        case StatTypeDuration::Month:
            return "mo";
        default:
            throw DataModellingException("DumpType<StatTypeDuration>::dump: Unexpected enum value for StatTypeDuration"
                                             + std::to_string(std::int64_t(v)),
                                         Here());
    }
}

std::string DumpType<StatTypeOperation>::dump(StatTypeOperation v) {
    switch (v) {
        case StatTypeOperation::Average:
            return "av";
        case StatTypeOperation::Max:
            return "mx";
        case StatTypeOperation::Min:
            return "mn";
        case StatTypeOperation::StandardDeviation:
            return "sd";
        default:
            throw DataModellingException(
                "DumpType<StatTypeOperation>::dump: Unexpected enum value for StatTypeOperation"
                    + std::to_string(std::int64_t(v)),
                Here());
    }
}

StatTypeDuration ParseType<StatTypeDuration>::parse(std::string_view val) {
    if (val == "da") {
        return StatTypeDuration::Day;
    }
    if (val == "mo") {
        return StatTypeDuration::Month;
    }
    throw DataModellingException(
        std::string("ParseType<StatTypeDuration>::parse Unknown value for StatTypeDuration: ") + std::string(val),
        Here());
}

StatTypeOperation ParseType<StatTypeOperation>::parse(std::string_view val) {
    if (val == "av") {
        return StatTypeOperation::Average;
    }
    if (val == "mx") {
        return StatTypeOperation::Max;
    }
    if (val == "mn") {
        return StatTypeOperation::Min;
    }
    if (val == "sd") {
        return StatTypeOperation::StandardDeviation;
    }
    throw DataModellingException(
        std::string("ParseType<StatTypeOperation>::parse Unknown value for StatTypeOperation: ") + std::string(val),
        Here());
}

std::string DumpType<SingleStatType>::dump(SingleStatType v) {
    return DumpType<StatTypeDuration>::dump(v.duration) + DumpType<StatTypeOperation>::dump(v.operation);
}

SingleStatType ParseType<SingleStatType>::parse(std::string_view val) {
    if (val.size() != 4) {
        throw DataModellingException(
            std::string("ParseType<SingleStatType>::parse Unknown value for SingleStatType: ") + std::string(val),
            Here());
    }
    return SingleStatType{ParseType<StatTypeDuration>::parse(val.substr(0, 2)),
                          ParseType<StatTypeOperation>::parse(val.substr(2, 2))};
}


std::string DumpType<StatType>::dump(StatType v) {
    if (v.secondLevel()) {
        return DumpType<SingleStatType>::dump(v.firstLevel()) + std::string("_")
             + DumpType<SingleStatType>::dump(*v.secondLevel());
    }
    return DumpType<SingleStatType>::dump(v.firstLevel());
}

StatType ParseType<StatType>::parse(std::string_view val) {
    switch (val.size()) {
        case 4:
            return StatType{ParseType<SingleStatType>::parse(val.substr(0, 4)), {}};
        case 9:
            if (val[4] != '_') {
                throw DataModellingException(std::string("ParseType<StatType>::parse Unknown value for StatType: ")
                                                 + std::string(val)
                                                 + std::string(". Expected a '_' as forth character."),
                                             Here());
            }
            return StatType{ParseType<SingleStatType>::parse(val.substr(0, 4)),
                            ParseType<SingleStatType>::parse(val.substr(5, 4))};
    }
    throw DataModellingException(
        std::string("ParseType<StatType>::parse Unknown value for StatType: ") + std::string(val), Here());
}


bool operator==(const StatType& lhs, const StatType& rhs) noexcept {
    return (lhs.firstLevel == rhs.firstLevel) && (lhs.secondLevel == rhs.secondLevel);
};
bool operator!=(const StatType& lhs, const StatType& rhs) noexcept {
    return !(lhs == rhs);
};

}  // namespace multio::datamod


namespace multio::util {

void util::Print<datamod::StatType>::print(PrintStream& ps, const datamod::StatType& t) {
    util::print(ps, datamod::TypeDumper<datamod::StatType>::dump(t));
}

}  // namespace multio::util

