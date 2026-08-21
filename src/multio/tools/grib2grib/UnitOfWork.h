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
/// @brief Work-unit abstraction for chunk-based MPI orchestration in `grib2grib`.

#pragma once

#include <cstdint>
#include <cstdio>
#include <memory>
#include <string>
#include <vector>

#include "multio/tools/grib2grib/ReaderContext.h"

namespace metkit::codes {
class CodesHandle;
}

namespace multio::distGrib1ToGrib2::grib2grib {

/// @brief Half-open byte range assigned to one worker for one input file.
///
/// The offsets describe a coarse scheduling slice, not guaranteed message
/// boundaries. `UnitOfWork` aligns the actual reading position to the first GRIB
/// message starting at or after `startOffset`.
struct WorkUnit {
    std::string filename;
    off_t startOffset = 0;  // Inclusive
    off_t endOffset = 0;    // Exclusive
};

/// @brief Query the size of one regular file in bytes.
/// @param path Absolute or relative path to the input file.
/// @return File size cast to `long` after validation.
/// @throw std::runtime_error If the path cannot be stat'ed, is not a regular file, or is too large.
long fileSizeBytes(const std::string& path);

/// @brief Split a file into at most `nChunks` contiguous byte ranges.
/// @param filename Input file to split.
/// @param nChunks Requested number of chunks before bounding by file size.
/// @return Half-open work-unit ranges covering the whole file.
/// @throw std::invalid_argument If `nChunks == 0`.
/// @throw std::runtime_error If the file size cannot be determined.
std::vector<WorkUnit> splitFileIntoNWorkUnits(const std::string& filename, std::size_t nChunks);

/// @brief Split a file into contiguous byte ranges not exceeding a reference size.
/// @param filename Input file to split.
/// @param maximumSizeBytes Maximum target size of each produced range.
/// @return Half-open work-unit ranges covering the whole file.
/// @throw std::invalid_argument If `maximumSizeBytes == 0`.
/// @throw std::runtime_error If the file size cannot be determined.
std::vector<WorkUnit> splitFileByMaximumWorkUnitSize(const std::string& filename, std::uint64_t maximumSizeBytes);

/// @brief Serialize one work-unit record into a compact binary payload.
/// @param workUnit Work-unit metadata to encode.
/// @return Binary payload suitable for MPI transfer or persistence.
std::vector<char> serializeWorkUnit(const WorkUnit& workUnit);

/// @brief Deserialize one work-unit record from a compact binary payload.
/// @param payload Binary payload previously produced by `serializeWorkUnit(...)`.
/// @return Reconstructed work-unit metadata.
/// @throw std::runtime_error If the payload is truncated or inconsistent.
WorkUnit deserializeWorkUnit(const std::vector<char>& payload);

/// @brief Serialize a sequence of work units into one compact binary payload.
/// @param workUnits Work-unit metadata sequence to encode.
/// @return Binary payload suitable for MPI transfer or persistence.
std::vector<char> serializeWorkUnits(const std::vector<WorkUnit>& workUnits);

/// @brief Deserialize a sequence of work units from a compact binary payload.
/// @param payload Binary payload previously produced by `serializeWorkUnits(...)`.
/// @return Reconstructed work-unit sequence.
/// @throw std::runtime_error If the payload is truncated or inconsistent.
std::vector<WorkUnit> deserializeWorkUnits(const std::vector<char>& payload);

/// @brief GRIB-message iterator bound to one scheduled work-unit byte range.
///
/// The class owns the open file handle and exposes a pull-based API that returns
/// one copied `CodesHandle` per message. Reading starts at the first GRIB message
/// whose start offset is at or after `workUnit.startOffset` and stops when the
/// next owned start offset is outside `[startOffset, endOffset)`.
///
/// Ownership is determined only by message start offset. A message claimed by a
/// unit may extend beyond `endOffset` and is still decoded fully by the owning
/// unit.
class UnitOfWork {
public:
    /// @brief Bind the reader to one immutable scheduled byte range.
    /// @param workUnit Scheduled file slice whose message starts are owned.
    /// @param readerMode Runtime strategy used to locate owned message starts.
    UnitOfWork(WorkUnit workUnit, WorkUnitReaderMode readerMode);

    /// @brief Close any open file handle on destruction.
    ~UnitOfWork() noexcept;

    UnitOfWork(const UnitOfWork&) = delete;
    UnitOfWork& operator=(const UnitOfWork&) = delete;
    UnitOfWork(UnitOfWork&&) = delete;
    UnitOfWork& operator=(UnitOfWork&&) = delete;

    /// @brief Expose the immutable scheduling metadata associated with this reader.
    /// @return Stored work-unit descriptor.
    const WorkUnit& workUnit() const noexcept;

    /// @brief Report the raw byte span covered by the scheduled range.
    /// @return `endOffset - startOffset` interpreted as an unsigned size.
    std::uint64_t theoreticalSize() const noexcept;

    /// @brief Open the file and align reading to the first GRIB message inside the range.
    /// @throw std::runtime_error If the file cannot be opened or GRIB scanning fails.
    void open();

    /// @brief Report whether another message may still be read from this range.
    /// @return `true` when the file is open and the current cursor is still before `endOffset`.
    bool newMessageAvailable() const noexcept;

    /// @brief Read and copy the next GRIB message inside the scheduled range.
    /// @return Owning `CodesHandle` copy for the next message, or `nullptr` when the range is exhausted.
    /// @throw std::runtime_error If file seeks, ecCodes decoding, or range checks fail.
    std::unique_ptr<metkit::codes::CodesHandle> nextMessage();

    /// @brief Close the underlying file handle and reset reader state.
    /// @return `true` when close succeeded or no file was open, otherwise `false`.
    bool close() noexcept;

private:
    const WorkUnit workUnit_;
    const WorkUnitReaderMode readerMode_;

    std::FILE* file_ = nullptr;
    /// Physical file end used by candidate-boundary validation.
    off_t fileEndOffset_ = 0;
    off_t currentOffset_ = 0;
    bool isOpen_ = false;
};

}  // namespace multio::distGrib1ToGrib2::grib2grib
