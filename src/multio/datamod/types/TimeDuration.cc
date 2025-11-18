/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "TimeDuration.h"

#include "eckit/utils/Overloaded.h"

#include "multio/datamod/core/DataModellingException.h"


namespace multio::datamod {

std::variant<std::int64_t, std::string> DumpType<TimeDuration>::dump(const TimeDuration& td) {
    using Ret = std::variant<std::int64_t, std::string>;
    return td.visit(eckit::Overloaded{
        [&](const std::chrono::hours& h) -> Ret { return h.count(); },
        [&](const std::chrono::seconds& s) -> Ret { return std::to_string(s.count()) + std::string("s"); }});
}

TimeDuration ParseType<TimeDuration>::parse(std::int64_t hours) noexcept {
    return std::chrono::hours{hours};
}
TimeDuration ParseType<TimeDuration>::parse(const std::string& val) {
    // TODO align these units with util/DateTime.h ??
    const auto throwInvalid
        = [&]() { throw DataModellingException(std::string("Invalid time duration: ") + val, Here()); };

    if (val.empty()) {
        throwInvalid();
    }

    // Check last char for unit
    char last = val.back();
    std::string unit;
    std::int64_t value;
    switch (last) {
        case 'h':
        case 's': {
            unit.assign(1, last);
            // Parse everything except last char as number
            if (val.size() == 1) {
                throwInvalid();
            }
            value = std::stol(val.substr(0, val.size() - 1));
        } break;
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9': {
            // No unit → entire string is number with hour units
            unit.assign(1, 'h');
            value = std::stol(val);
        } break;
        default:
            throwInvalid();
    }

    switch (unit[0]) {
        case 'h':
            return std::chrono::hours{value};
        case 's':
            return std::chrono::seconds{value};
        default:
            throw DataModellingException(std::string("Invalid unit in time duration: ") + val, Here());
    }
}


}  // namespace multio::datamod


namespace multio::util {

void util::Print<datamod::TimeDuration>::print(PrintStream& ps, const datamod::TimeDuration& t) {
    util::print(ps, datamod::TypeDumper<datamod::TimeDuration>::dump(t));
}


}  // namespace multio::util
