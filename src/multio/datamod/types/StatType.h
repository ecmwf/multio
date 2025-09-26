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

#include "multio/datamod/core/DataModellingException.h"
#include "multio/datamod/core/TypeParserDumper.h"

#include "multio/util/Hash.h"
#include "multio/util/Print.h"
#include "multio/util/TypeToString.h"


namespace multio::datamod {

//-----------------------------------------------------------------------------


enum class StatTypeDuration : std::size_t
{
    Day = 0,
    Month = 1,
};

// Define an order on StatTypeDuration. For multiple level operations, the first level must have greatest order
bool operator>=(StatTypeDuration lhs, StatTypeDuration rhs) noexcept;
bool operator<=(StatTypeDuration lhs, StatTypeDuration rhs) noexcept;
bool operator>(StatTypeDuration lhs, StatTypeDuration rhs) noexcept;
bool operator<(StatTypeDuration lhs, StatTypeDuration rhs) noexcept;

enum class StatTypeOperation : std::size_t
{
    Average,
    Max,
    Min,
    StandardDeviation
};


struct SingleStatType {
    StatTypeDuration duration;
    StatTypeOperation operation;
};


class StatType {
public:
    // Only constructor - will validate requirements on StatType
    StatType(SingleStatType first, std::optional<SingleStatType> second = {});

    SingleStatType firstLevel() const;
    std::optional<SingleStatType> secondLevel() const;

    // Returns the number of levels (1 or 2)
    std::size_t levels() const;

private:
    SingleStatType firstLevel_;
    std::optional<SingleStatType> secondLevel_{};
};

bool operator==(const StatType& lhs, const StatType& rhs) noexcept;
bool operator!=(const StatType& lhs, const StatType& rhs) noexcept;


}  // namespace multio::datamod


namespace std {

template <>
struct hash<multio::datamod::SingleStatType> {
    std::size_t operator()(const multio::datamod::SingleStatType& o) const noexcept {
        return multio::util::hashCombine(multio::util::hash(o.duration), multio::util::hash(o.operation));
    }
};

template <>
struct hash<multio::datamod::StatType> {
    std::size_t operator()(const multio::datamod::StatType& o) const noexcept {
        return multio::util::hashCombine(multio::util::hash(o.firstLevel()), multio::util::hash(o.secondLevel()));
    }
};
}  // namespace std


namespace multio::util {

template <>
struct Print<datamod::StatType> {
    static void print(PrintStream& ps, const datamod::StatType& v);
};

template <>
struct TypeToString<datamod::StatType> {
    std::string operator()() const { return "datamod::StatType"; };
};

}  // namespace multio::util

namespace multio::datamod {

template <>
struct DumpType<StatTypeDuration> {
    static std::string dump(StatTypeDuration);
};

template <>
struct ParseType<StatTypeDuration> {
    static inline StatTypeDuration parse(StatTypeDuration v) noexcept { return v; };
    static StatTypeDuration parse(std::string_view s);
};


template <>
struct DumpType<StatTypeOperation> {
    static std::string dump(StatTypeOperation);
};

template <>
struct ParseType<StatTypeOperation> {
    static inline StatTypeOperation parse(StatTypeOperation v) noexcept { return v; };
    static StatTypeOperation parse(std::string_view s);
};


template <>
struct DumpType<SingleStatType> {
    static std::string dump(SingleStatType);
};

template <>
struct ParseType<SingleStatType> {
    static inline SingleStatType parse(SingleStatType v) noexcept { return v; };
    static SingleStatType parse(std::string_view s);
};


template <>
struct DumpType<StatType> {
    static std::string dump(StatType);
};

template <>
struct ParseType<StatType> {
    static inline StatType parse(StatType v) noexcept { return v; };
    static StatType parse(std::string_view s);
};


}  // namespace multio::datamod

