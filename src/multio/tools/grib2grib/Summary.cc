/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/// @file
/// @brief Root-side summary utilities for `grib2grib` outcomes.

#include "multio/tools/grib2grib/Summary.h"

#include <map>
#include <utility>

namespace multio::distGrib1ToGrib2::grib2grib {

std::vector<FileStageOutcomes> createPerFileOutcomes(const std::vector<FileStageOutcomes>& outcomesPerWorkUnit) {
    std::map<std::string, FileStageOutcomes> grouped;

    for (const auto& workUnitOutcome : outcomesPerWorkUnit) {
        auto [it, inserted] = grouped.emplace(workUnitOutcome.filename, FileStageOutcomes{});
        if (inserted) {
            it->second.filename = workUnitOutcome.filename;
        }
        it->second.add(workUnitOutcome);
    }

    std::vector<FileStageOutcomes> perFileOutcomes;
    perFileOutcomes.reserve(grouped.size());
    for (auto& [filename, outcome] : grouped) {
        (void)filename;
        perFileOutcomes.push_back(std::move(outcome));
    }

    return perFileOutcomes;
}

}  // namespace multio::distGrib1ToGrib2::grib2grib
