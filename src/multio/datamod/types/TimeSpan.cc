#include "TimeSpan.h"


namespace multio::datamod {

std::variant<std::int64_t, std::string> DumpType<TimeSpan>::dump(const TimeSpan& value) {
    if (value.isNone()) {
        return std::string{"none"};
    }

    if (value.isFromStart()) {
        return std::string{"fs"};
    }

    return TypeDumper<TimeDuration>::dump(value.duration());
}


TimeSpan ParseType<TimeSpan>::parse(std::int64_t hours) noexcept {
    return TimeSpan{TimeDuration{std::chrono::hours{hours}}};
}


TimeSpan ParseType<TimeSpan>::parse(const std::string& value) {
    if (value == "none") {
        return TimeSpan::none();
    }

    if (value == "fs") {
        return TimeSpan::fromStart();
    }

    throw DataModellingException(
        std::string{"Invalid timespan value: "} + value + ". Only integer hours, string \"none\" or string \"fs\" are supported.",
        Here());
}

}  // namespace multio::datamod


namespace multio::util {

void Print<datamod::TimeSpan>::print(PrintStream& ps, const datamod::TimeSpan& value) {
    util::print(ps, datamod::TypeDumper<datamod::TimeSpan>::dump(value));
}

}  // namespace multio::util