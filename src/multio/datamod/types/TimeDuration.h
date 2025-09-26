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

#include "multio/datamod/core/TypeParserDumper.h"

#include "multio/util/Hash.h"
#include "multio/util/Print.h"
#include "multio/util/TypeToString.h"

#include <chrono>
#include <string>
#include <variant>


namespace multio::datamod {

//-----------------------------------------------------------------------------

using IntOrString = std::variant<std::int64_t, std::string>;

using TimeDurationVariant = std::variant<std::chrono::hours, std::chrono::seconds>;

// Time duration will get a proper type because printing it has a special domain specific meaning
// that goes beyond chrono::hours and chrono::seconds
class TimeDuration : public TimeDurationVariant {
public:
    using TimeDurationVariant::TimeDurationVariant;
    using TimeDurationVariant::operator=;

    TimeDuration(const TimeDuration& other) = default;
    TimeDuration(TimeDuration&&) noexcept = default;

    TimeDuration& operator=(const TimeDuration& other) = default;
    TimeDuration& operator=(TimeDuration&& other) noexcept = default;

    template <typename Func>
    decltype(auto) visit(Func&& func) const {
        return std::visit(std::forward<Func>(func), static_cast<const TimeDurationVariant&>(*this));
    }
    template <typename Func>
    decltype(auto) visit(Func&& func) {
        return std::visit(std::forward<Func>(func), static_cast<TimeDurationVariant&>(*this));
    }

    std::int64_t toSeconds() const {
        return visit([](auto&& duration) -> std::int64_t {
            return std::chrono::duration_cast<std::chrono::seconds>(duration).count();
        });
    }
};

}  // namespace multio::datamod


namespace std {
template <>
struct variant_size<multio::datamod::TimeDuration> : variant_size<multio::datamod::TimeDurationVariant> {};

template <std::size_t I>
struct variant_alternative<I, multio::datamod::TimeDuration>
    : variant_alternative<I, multio::datamod::TimeDurationVariant> {};
}  // namespace std

//-----------------------------------------------------------------------------

namespace multio::util {

template <>
struct Print<datamod::TimeDuration> {
    static void print(PrintStream& ps, const datamod::TimeDuration& v);
};


template <>
struct TypeToString<datamod::TimeDuration> {
    std::string operator()() const { return "datamod::TimeDuration"; };
};

}  // namespace multio::util

namespace multio::datamod {

template <>
struct DumpType<TimeDuration> {
    static IntOrString dump(const TimeDuration&);
};


template <>
struct ParseType<TimeDuration> {
    static TimeDuration parse(std::int64_t hours) noexcept;
    static TimeDuration parse(const std::string& s);
};


}  // namespace multio::datamod


template <>
struct std::hash<multio::datamod::TimeDuration> {
    std::size_t operator()(const multio::datamod::TimeDuration& td) const noexcept {
        return td.visit(eckit::Overloaded{
            [&](const std::chrono::hours& h) { return multio::util::hashCombine(h.count(), 'h'); },
            [&](const std::chrono::seconds& s) { return multio::util::hashCombine(s.count(), 's'); }});
    }
};
