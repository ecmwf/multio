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

#include <tuple>
#include <type_traits>

#include "multio/datamod/core/Entry.h"
#include "multio/datamod/core/EntryDef.h"
#include "multio/datamod/core/Record.h"

#include "multio/util/Print.h"


namespace multio::util {

// Printing something readable to ostream
template <typename ValueType, typename Mapper>
struct Print<datamod::Entry<ValueType, Mapper>> {
    static void print(PrintStream& ps, const datamod::Entry<ValueType, Mapper>& entry) {
        using ParserDumper = typename datamod::Entry<ValueType, Mapper>::ParserDumper;

        if (!entry.isSet()) {
            ps << "<UNSET>";
        }
        else {
            ps << ParserDumper::template dumpTo<std::ostream>(entry.get());
        }
    }
};


template <typename RecordType>
struct Print<datamod::ScopedRecord<RecordType>> {
    static void print(PrintStream& ps, const datamod::ScopedRecord<RecordType>& rec) {
        ps << static_cast<const RecordType&>(rec);
    }
};

}  // namespace multio::util

namespace multio::datamod {

struct PrintRecord {
    // TODO maybe add feature to Printstream about verbosity
    static constexpr bool DETAILED = false;
    template <typename Entry_>
    static void printEntry(util::PrintStream& ps, bool& isFirst, std::string_view key, const Entry_& entry) {
        if (!DETAILED && !entry.isSet())
            return;

        if (isFirst) {
            isFirst = false;
        }
        else {
            ps << ", ";
        }

        ps << key << ": ";
        if (IsRecord_v<EntryValueType_t<Entry_>>) {
            ps.softBreak();
        }
        {
            util::IndentGuard g(ps);
            ps << entry;
        }
        ps.softBreak();
    }

    template <typename Record>
    static void print(util::PrintStream& ps, const Record& rec) {
        ps << "{ ";
        bool first = true;
        std::apply([&](const auto&... entryDef) { (printEntry(ps, first, entryDef.key(), entryDef.get(rec)), ...); },
                   recordEntries(rec));
        ps << "}";
    }
};

}  // namespace multio::datamod

