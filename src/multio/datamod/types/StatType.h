/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#pragma once

#include <cstddef>
#include <optional>
#include <string>
#include <string_view>

#include "multio/util/Hash.h"


namespace multio::datamod {


enum class StatTypeDuration : std::size_t
{
    Day = 0,
    Month = 1,
};

bool operator>=(StatTypeDuration lhs, StatTypeDuration rhs) noexcept;
bool operator<=(StatTypeDuration lhs, StatTypeDuration rhs) noexcept;
bool operator>(StatTypeDuration lhs, StatTypeDuration rhs) noexcept;
bool operator<(StatTypeDuration lhs, StatTypeDuration rhs) noexcept;

std::string statTypeDurationToString(StatTypeDuration v);
StatTypeDuration statTypeDurationFromString(std::string_view s);


enum class StatTypeOperation : std::size_t
{
    Average,
    Max,
    Min,
    StandardDeviation
};

std::string statTypeOperationToString(StatTypeOperation v);
StatTypeOperation statTypeOperationFromString(std::string_view s);


struct SingleStatType {
    StatTypeDuration duration;
    StatTypeOperation operation;

    std::string toString() const;
    static SingleStatType fromString(std::string_view s);
};

bool operator==(const SingleStatType& lhs, const SingleStatType& rhs) noexcept;


class StatType {
public:
    StatType(SingleStatType first, std::optional<SingleStatType> second = {});

    SingleStatType firstLevel() const;
    std::optional<SingleStatType> secondLevel() const;
    std::size_t levels() const;

    std::string toString() const;
    static StatType fromString(std::string_view s);

private:
    SingleStatType firstLevel_;
    std::optional<SingleStatType> secondLevel_{};
};

bool operator==(const StatType& lhs, const StatType& rhs) noexcept;
bool operator!=(const StatType& lhs, const StatType& rhs) noexcept;
std::ostream& operator<<(std::ostream&, const StatType&);

}  // namespace multio::datamod


template <>
struct std::hash<multio::datamod::SingleStatType> {
    std::size_t operator()(const multio::datamod::SingleStatType& o) const noexcept {
        return multio::util::hashCombine(multio::util::hash(o.duration), multio::util::hash(o.operation));
    }
};

template <>
struct std::hash<multio::datamod::StatType> {
    std::size_t operator()(const multio::datamod::StatType& o) const noexcept {
        return multio::util::hashCombine(multio::util::hash(o.firstLevel()), multio::util::hash(o.secondLevel()));
    }
};
