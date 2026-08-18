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

#pragma once

#include <cstddef>
#include <vector>

#include "multio/tools/grib2grib/StageOutcomes.h"

namespace multio::distGrib1ToGrib2::grib2grib {

/// @brief Aggregate file-bucket statistics for one `FileSummary` class.
struct AggregateSummaryBucket {
    std::size_t nFiles = 0;
    std::size_t nMessages = 0;
};

/// @brief Aggregate summary of the final per-file classification.
///
/// Counts are grouped by file outcome class. Percentages are not stored here.
/// They are derived at print time from the file counters.
struct AggregateSummary {
    AggregateSummaryBucket success;
    AggregateSummaryBucket partial;
    AggregateSummaryBucket fail;
};

/// @brief Group work-unit outcomes by filename.
///
/// The returned vector is filename-sorted because the implementation groups via
/// `std::map<std::string, FileStageOutcomes>`.
std::vector<FileStageOutcomes> createPerFileOutcomes(const std::vector<FileStageOutcomes>& outcomesPerWorkUnit);

/// @brief Aggregate final file-status buckets from the per-file summary.
AggregateSummary summarizeByFileSummary(const std::vector<FileStageOutcomes>& summary);

}  // namespace multio::distGrib1ToGrib2::grib2grib
