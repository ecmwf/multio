#pragma once

#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <variant>

#include "multio/datamod/core/TypeParserDumper.h"
#include "multio/datamod/types/TimeDuration.h"
#include "multio/util/Hash.h"
#include "multio/util/Print.h"

#include "multio/datamod/core/DataModellingException.h"

namespace multio::datamod {

class TimeSpan {
public:
    TimeSpan() = default;

    explicit TimeSpan(TimeDuration duration) :
        value_{std::move(duration)} {}

    static TimeSpan none() {
        TimeSpan ret;
        ret.value_ = std::monostate{};
        return ret;
    }

    bool isNone() const {
        return std::holds_alternative<std::monostate>(value_);
    }

    bool isDuration() const {
        return std::holds_alternative<TimeDuration>(value_);
    }

    const TimeDuration& duration() const {
        if (!isDuration()) {
            throw DataModellingException("TimeSpan does not contain a duration", Here());
        }
        return std::get<TimeDuration>(value_);
    }

    std::int64_t toHours() const {
        return duration().toHours();
    }

    std::int64_t toSeconds() const {
        return duration().toSeconds();
    }

friend bool operator==(const TimeSpan& lhs, const TimeSpan& rhs) {
    if (lhs.isNone() || rhs.isNone()) {
        return lhs.isNone() && rhs.isNone();
    }

    return lhs.toSeconds() == rhs.toSeconds();
}

friend bool operator!=(const TimeSpan& lhs, const TimeSpan& rhs) {
    return !(lhs == rhs);
}

private:
    std::variant<TimeDuration, std::monostate> value_{std::chrono::hours{0}};

    friend struct DumpType<TimeSpan>;
    friend struct ParseType<TimeSpan>;
};

template <>
struct DumpType<TimeSpan> {
    static std::variant<std::int64_t, std::string> dump(const TimeSpan&);
};

template <>
struct ParseType<TimeSpan> {
    static TimeSpan parse(std::int64_t hours) noexcept;
    static TimeSpan parse(const std::string& value);
};

}  // namespace multio::datamod


template <>
struct multio::util::Print<multio::datamod::TimeSpan> {
    static void print(PrintStream& ps, const datamod::TimeSpan& v);
};


template <>
struct std::hash<multio::datamod::TimeSpan> {
    std::size_t operator()(const multio::datamod::TimeSpan& v) const noexcept {
        if (v.isNone()) {
            return multio::util::hashCombine(
                std::string{"TimeSpan"},
                std::string{"none"}
            );
        }

        return multio::util::hashCombine(
            std::string{"TimeSpan"},
            v.duration()
        );
    }
};