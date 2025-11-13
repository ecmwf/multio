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

#include <cstdint>
#include <functional>
#include <ostream>
#include <string>

#include "multio/datamod/core/TypeParserDumper.h"
#include "multio/util/Print.h"


namespace multio::datamod {

class Param {
public:
    Param(const Param&);
    Param(const std::int64_t);
    Param(const std::string&);

    Param& operator=(const Param&);
    Param& operator=(const std::int64_t);
    Param& operator=(const std::string&);

    std::int64_t id() const { return id_; };

private:
    std::int64_t id_;
};

bool operator==(const Param&, const Param&) noexcept;
bool operator<=(const Param&, const Param&) noexcept;
bool operator>=(const Param&, const Param&) noexcept;
std::ostream& operator<<(std::ostream&, const Param&);

template <>
struct DumpType<Param> {
    static std::int64_t dump(const Param& param) { return param.id(); }
};

}  // namespace multio::datamod


template <>
struct multio::util::Print<multio::datamod::Param> {
    static void print(PrintStream& ps, const multio::datamod::Param& v);
};


template <>
struct std::hash<multio::datamod::Param> {
    size_t operator()(const multio::datamod::Param& param) const { return param.id(); }
};
