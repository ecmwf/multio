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

#include "eckit/exception/Exceptions.h"


namespace multio::datamod {

StatType::StatType(SingleStatType first, std::optional<SingleStatType> second) :
    firstLevel_(first), secondLevel_(second) {
    if (secondLevel_ && !(firstLevel_.duration > secondLevel_->duration)) {
        throw eckit::UserError("StatType: Invalid level combination: " + toString(), Here());
    }
}

SingleStatType StatType::firstLevel() const {
    return firstLevel_;
}
std::optional<SingleStatType> StatType::secondLevel() const {
    return secondLevel_;
}
std::size_t StatType::levels() const {
    return (secondLevel_ ? 2 : 1);
}


// --- StatTypeDuration ---

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

std::string statTypeDurationToString(StatTypeDuration v) {
    switch (v) {
        case StatTypeDuration::Day:
            return "da";
        case StatTypeDuration::Month:
            return "mo";
        default:
            throw eckit::UserError("statTypeDurationToString: Unexpected enum value " + std::to_string(std::int64_t(v)),
                                   Here());
    }
}

StatTypeDuration statTypeDurationFromString(std::string_view val) {
    if (val == "da")
        return StatTypeDuration::Day;
    if (val == "mo")
        return StatTypeDuration::Month;
    throw eckit::UserError("statTypeDurationFromString: Unknown value: " + std::string(val), Here());
}


// --- StatTypeOperation ---

std::string statTypeOperationToString(StatTypeOperation v) {
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
            throw eckit::UserError(
                "statTypeOperationToString: Unexpected enum value " + std::to_string(std::int64_t(v)), Here());
    }
}

StatTypeOperation statTypeOperationFromString(std::string_view val) {
    if (val == "av")
        return StatTypeOperation::Average;
    if (val == "mx")
        return StatTypeOperation::Max;
    if (val == "mn")
        return StatTypeOperation::Min;
    if (val == "sd")
        return StatTypeOperation::StandardDeviation;
    throw eckit::UserError("statTypeOperationFromString: Unknown value: " + std::string(val), Here());
}


// --- SingleStatType ---

std::string SingleStatType::toString() const {
    return statTypeDurationToString(duration) + statTypeOperationToString(operation);
}

SingleStatType SingleStatType::fromString(std::string_view val) {
    if (val.size() != 4) {
        throw eckit::UserError("SingleStatType::fromString: Expected 4 characters, got: " + std::string(val), Here());
    }
    return SingleStatType{statTypeDurationFromString(val.substr(0, 2)), statTypeOperationFromString(val.substr(2, 2))};
}

bool operator==(const SingleStatType& lhs, const SingleStatType& rhs) noexcept {
    return lhs.duration == rhs.duration && lhs.operation == rhs.operation;
}


// --- StatType ---

std::string StatType::toString() const {
    if (secondLevel_) {
        return firstLevel_.toString() + "_" + secondLevel_->toString();
    }
    return firstLevel_.toString();
}

StatType StatType::fromString(std::string_view val) {
    switch (val.size()) {
        case 4:
            return StatType{SingleStatType::fromString(val.substr(0, 4)), {}};
        case 9:
            if (val[4] != '_') {
                throw eckit::UserError("StatType::fromString: Expected '_' at position 4 in: " + std::string(val),
                                       Here());
            }
            return StatType{SingleStatType::fromString(val.substr(0, 4)), SingleStatType::fromString(val.substr(5, 4))};
        default:
            throw eckit::UserError("StatType::fromString: Unknown value: " + std::string(val), Here());
    }
}

bool operator==(const StatType& lhs, const StatType& rhs) noexcept {
    return (lhs.firstLevel() == rhs.firstLevel()) && (lhs.secondLevel() == rhs.secondLevel());
}
bool operator!=(const StatType& lhs, const StatType& rhs) noexcept {
    return !(lhs == rhs);
}

std::ostream& operator<<(std::ostream& out, const StatType& st) {
    out << st.toString();
    return out;
}

}  // namespace multio::datamod
