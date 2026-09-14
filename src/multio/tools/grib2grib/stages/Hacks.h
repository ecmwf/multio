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
/// @brief Temporary diagnostic hooks for the isolated `grib2grib` pipeline.

#pragma once

namespace metkit::codes {
class CodesHandle;
}

namespace multio::sink {
class DataSink;
}

namespace multio::distGrib1ToGrib2::grib2grib {

/// @brief Run temporary diagnostics before the encoded output is sunk.
///
/// All failures are trapped so diagnostic code cannot interrupt normal output.
void runHacksStage(multio::sink::DataSink* sink, const metkit::codes::CodesHandle& inputGribMessage,
                   const metkit::codes::CodesHandle& outputGribMessage) noexcept;

}  // namespace multio::distGrib1ToGrib2::grib2grib
