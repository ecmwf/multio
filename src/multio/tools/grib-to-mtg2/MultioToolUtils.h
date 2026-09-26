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
/// @brief Tool-level orchestration helpers for the distributed `grib-to-mtg2` tool.

#pragma once

#include <string>
#include <vector>

#include "eckit/mpi/Comm.h"

#include "multio/tools/grib-to-mtg2/GlobalContext.h"
#include "multio/tools/grib-to-mtg2/Sink.h"
#include "multio/tools/grib-to-mtg2/StageOutcomes.h"
#include "multio/tools/grib-to-mtg2/Summary.h"
#include "multio/tools/grib-to-mtg2/UnitOfWork.h"
#include "multio/tools/grib-to-mtg2/WorkUnitLoadBalancer.h"

namespace multio::grib_to_mtg2::utils {

using GlobalContext = multio::grib_to_mtg2::GlobalContext;
using WorkUnit = multio::grib_to_mtg2::WorkUnit;
using WorkBucket = multio::grib_to_mtg2::WorkBucket;
using FileStageOutcomes = multio::grib_to_mtg2::FileStageOutcomes;
using GribToMtg2Sinks = multio::grib_to_mtg2::GribToMtg2Sinks;
using SummaryType = std::vector<FileStageOutcomes>;
using AggregateSummaryBucket = multio::grib_to_mtg2::AggregateSummaryBucket;
using AggregateSummary = multio::grib_to_mtg2::AggregateSummary;

eckit::LocalConfiguration loadAndBroadcastOptionsAsConfiguration(const std::string& optionsFile,
                                                                 const eckit::mpi::Comm& comm);

GlobalContext buildGlobalContext(const eckit::LocalConfiguration& rawOptions);

std::unique_ptr<GribToMtg2Sinks> buildRankLocalWriter(const eckit::LocalConfiguration& rawOptions,
                                                      const GlobalContext& context, const std::string& outputDirectory,
                                                      const eckit::mpi::Comm& comm);

std::vector<WorkUnit> distributeWork(const std::string& fileList, long averageWorkUnitsPerRank,
                                     const eckit::mpi::Comm& comm);

std::vector<FileStageOutcomes> processWorkUnits(const std::vector<WorkUnit>& workUnits, const GlobalContext& context,
                                                GribToMtg2Sinks& writer);

std::vector<FileStageOutcomes> gatherWorkUnitOutcome(const std::vector<FileStageOutcomes>& localOutcomes,
                                                     const eckit::mpi::Comm& comm);

std::vector<FileStageOutcomes> summarizeWorkUnitOutcomePerFile(
    const std::vector<FileStageOutcomes>& workUnitOutcomeGlobal);

SummaryType createSummary(const std::vector<FileStageOutcomes>& workUnitOutcomePerFile);

AggregateSummary buildAggregateSummary(const SummaryType& summary);

void writeSummary(const SummaryType& summary, const std::string& outputDirectory);

void printAggregateSummary(const AggregateSummary& summary);

}  // namespace multio::grib_to_mtg2::utils
