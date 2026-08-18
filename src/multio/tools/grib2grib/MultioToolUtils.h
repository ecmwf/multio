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
/// @brief Tool-level orchestration helpers for the distributed `grib2grib` tool.

#pragma once

#include <string>
#include <vector>

#include "eckit/mpi/Comm.h"

#include "multio/tools/grib2grib/GlobalContext.h"
#include "multio/tools/grib2grib/Sink.h"
#include "multio/tools/grib2grib/StageOutcomes.h"
#include "multio/tools/grib2grib/Summary.h"
#include "multio/tools/grib2grib/UnitOfWork.h"
#include "multio/tools/grib2grib/WorkUnitLoadBalancer.h"

namespace multio::grib2grib::utils {

using GlobalContext = multio::distGrib1ToGrib2::grib2grib::GlobalContext;
using WorkUnit = multio::distGrib1ToGrib2::grib2grib::WorkUnit;
using WorkBucket = multio::distGrib1ToGrib2::grib2grib::WorkBucket;
using FileStageOutcomes = multio::distGrib1ToGrib2::grib2grib::FileStageOutcomes;
using Grib2GribSinks = multio::distGrib1ToGrib2::grib2grib::Grib2GribSinks;
using SummaryType = std::vector<FileStageOutcomes>;
using AggregateSummaryBucket = multio::distGrib1ToGrib2::grib2grib::AggregateSummaryBucket;
using AggregateSummary = multio::distGrib1ToGrib2::grib2grib::AggregateSummary;

eckit::LocalConfiguration loadAndBroadcastOptionsAsConfiguration(const std::string& optionsFile,
                                                                 const eckit::mpi::Comm& comm);

GlobalContext buildGlobalContext(const eckit::LocalConfiguration& rawOptions);

std::unique_ptr<Grib2GribSinks> buildRankLocalWriter(const eckit::LocalConfiguration& rawOptions,
                                                     const GlobalContext& context, const std::string& outputDirectory,
                                                     const eckit::mpi::Comm& comm);

std::vector<WorkUnit> distributeWork(const std::string& fileList, long averageWorkUnitsPerRank,
                                     const eckit::mpi::Comm& comm);

std::vector<FileStageOutcomes> processWorkUnits(const std::vector<WorkUnit>& workUnits, const GlobalContext& context,
                                                Grib2GribSinks& writer);

std::vector<FileStageOutcomes> gatherWorkUnitOutcome(const std::vector<FileStageOutcomes>& localOutcomes,
                                                     const eckit::mpi::Comm& comm);

std::vector<FileStageOutcomes> summarizeWorkUnitOutcomePerFile(
    const std::vector<FileStageOutcomes>& workUnitOutcomeGlobal);

SummaryType createSummary(const std::vector<FileStageOutcomes>& workUnitOutcomePerFile);

AggregateSummary buildAggregateSummary(const SummaryType& summary);

void writeSummary(const SummaryType& summary, const std::string& outputDirectory);

void printAggregateSummary(const AggregateSummary& summary);

}  // namespace multio::grib2grib::utils
