/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#pragma once

#include <type_traits>
#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/core/Compare.h"
#include "multio/datamod/core/EntryDumper.h"
#include "multio/datamod/core/EntryParser.h"
#include "multio/datamod/core/Record.h"
#include "multio/mars2grib/Mars2GribException.h"

#include "multio/util/Print.h"
#include "multio/util/TypeToString.h"
#include "multio/util/TypeTraits.h"

// Level config
namespace multio::mars2grib::grib2 {

namespace dm = multio::datamod;


template <typename T>
struct Grib2Equal {
    bool operator()(const T& lhs, const T& rhs) const { return lhs == rhs; }
};


template <>
struct Grib2Equal<double> {
    bool operator()(const double& lhs, const double& rhs) const {
        // TODO(pgeier) Use composition types to carry on precision information
        // Compare below micro precision
        // Most grib2 floating point values compare with that
        // A few might have more detailed precision
        constexpr double EPS = 5e-7;
        return lhs <= (rhs + EPS) && lhs >= (rhs - EPS);
    }
};


template <typename ValueType>
struct Grib2Equal<dm::Entry<ValueType>> {
    using Type = dm::Entry<ValueType>;
    bool operator()(const Type& lhs, const Type& rhs) const {
        return (!lhs.isSet() && !rhs.isSet())
            || (lhs.isSet() && rhs.isSet() && (Grib2Equal<ValueType>{}(lhs.get(), rhs.get())));
    }
};


template <typename T>
bool grib2Equal(const T& lhs, const T& rhs) {
    return Grib2Equal<T>{}(lhs, rhs);
}


template <typename RecType>
bool compareGrib2Keys(const RecType& lhs, const RecType& rhs) {
    return std::apply(
        [&](const auto&... entryDef) { return (grib2Equal(entryDef.get(lhs), entryDef.get(rhs)) && ... && true); },
        dm::recordEntries<RecType>());
}


template <typename RecType>
void testReadback(metkit::codes::CodesHandle& handle, const RecType& rec) {
    auto readback = dm::readRecord<RecType>(handle);

    if (!compareGrib2Keys(readback, rec)) {
        std::ostringstream oss;
        util::PrintStream ps(oss);
        ps << "Readback test failed for " << dm::RecordName_v<RecType> << ". Keys written " << std::endl;
        {
            util::IndentGuard g(ps);
            ps << rec << std::endl;
        }

        ps << "differ from keys read " << std::endl;
        {
            util::IndentGuard g(ps);
            ps << readback << std::endl;
        }

        ps << "Differences: " << std::endl;
        {
            util::IndentGuard g(ps);
            util::forEach(
                [&](const auto& entryDef) {
                    const auto& lhs = entryDef.get(rec);
                    const auto& rhs = entryDef.get(readback);
                    if (!grib2Equal(lhs, rhs)) {
                        ps << entryDef.key() << ": " << lhs << " != " << rhs << std::endl;

                        using ElementType = std::decay_t<decltype(lhs.get())>;
                        if constexpr (std::is_arithmetic_v<ElementType>) {
                            if (lhs.isSet() && rhs.isSet()) {
                                util::IndentGuard g2(ps);
                                ps << "diff: " << (lhs.get() - rhs.get()) << std::endl;
                            }
                        }
                    }
                },
                dm::recordEntries<RecType>());
        }

        throw Mars2GribException(oss.str(), Here());
    }
}


template <typename RecType>
void writeKeys(const RecType& rec, metkit::codes::CodesHandle& handle, bool readbackTest = false) {
    dm::DumpOptions dumpOpts;
    dumpOpts.removeMissingKeys = true;
    dm::dumpRecord(rec, handle, dumpOpts);

    if (readbackTest) {
        testReadback(handle, rec);
    }
}


};  // namespace multio::mars2grib::grib2
