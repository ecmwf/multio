#pragma once

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

namespace {
    enum TimeSpanVariant {
        None,
        FromStart,
        Duration,
    };
}

class TimeSpan {
public:
    TimeSpan() = default;

    explicit TimeSpan(TimeDuration duration) : variant_{TimeSpanVariant::Duration}, duration_{std::move(duration)} {}

    static TimeSpan none() {
        TimeSpan ret;
        ret.variant_ = TimeSpanVariant::None;
        return ret;
    }

    static TimeSpan fromStart() {
        TimeSpan ret;
        ret.variant_ = TimeSpanVariant::FromStart;
        return ret;
    }

    bool isNone() const { return variant_ == TimeSpanVariant::None; }

    bool isFromStart() const { return variant_ == TimeSpanVariant::FromStart; }

    bool isDuration() const { return variant_ == TimeSpanVariant::Duration; }

    const TimeDuration& duration() const {
        if (!isDuration()) {
            throw DataModellingException("TimeSpan does not contain a duration", Here());
        }
        return *duration_;
    }

    std::int64_t toHours() const { return duration().toHours(); }

    std::int64_t toSeconds() const { return duration().toSeconds(); }

    friend bool operator==(const TimeSpan& lhs, const TimeSpan& rhs) {
        if (lhs.isNone() || rhs.isNone()) {
            return lhs.isNone() && rhs.isNone();
        }

        if (lhs.isFromStart() || rhs.isFromStart()) {
            return lhs.isFromStart() && rhs.isFromStart();
        }

        return lhs.toSeconds() == rhs.toSeconds();
    }

    friend bool operator!=(const TimeSpan& lhs, const TimeSpan& rhs) { return !(lhs == rhs); }

private:
    TimeSpanVariant variant_{None};
    std::optional<TimeDuration> duration_{std::nullopt};

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
            return multio::util::hashCombine(std::string{"TimeSpan"}, std::string{"none"});
        }

        if (v.isFromStart()) {
            return multio::util::hashCombine(std::string{"TimeSpan"}, std::string{"fs"});
        }

        return multio::util::hashCombine(std::string{"TimeSpan"}, v.duration());
    }
};