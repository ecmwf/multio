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

namespace metkit::codes {
class CodesHandle;
}

namespace multio::distGrib1ToGrib2::grib2grib {

struct WorkUnit {
    std::string filename;
    off_t startOffset = 0;  // Inclusive
    off_t endOffset   = 0;  // Exclusive
};

long fileSizeBytes(const std::string& path);

std::vector<WorkUnit> splitFileIntoNWorkUnits(const std::string& filename, std::size_t nChunks);
std::vector<WorkUnit> splitFileByMaximumWorkUnitSize(const std::string& filename, std::uint64_t maximumSizeBytes);

std::vector<char> serializeWorkUnit(const WorkUnit& workUnit);
WorkUnit deserializeWorkUnit(const std::vector<char>& payload);

std::vector<char> serializeWorkUnits(const std::vector<WorkUnit>& workUnits);
std::vector<WorkUnit> deserializeWorkUnits(const std::vector<char>& payload);

class UnitOfWork {
public:
    explicit UnitOfWork(WorkUnit workUnit);
    ~UnitOfWork() noexcept;

    UnitOfWork(const UnitOfWork&) = delete;
    UnitOfWork& operator=(const UnitOfWork&) = delete;
    UnitOfWork(UnitOfWork&&) = delete;
    UnitOfWork& operator=(UnitOfWork&&) = delete;

    const WorkUnit& workUnit() const noexcept;
    std::uint64_t theoreticalSize() const noexcept;

    void open();
    bool newMessageAvailable() const noexcept;
    std::unique_ptr<metkit::codes::CodesHandle> nextMessage();
    bool close() noexcept;

private:
    const WorkUnit workUnit_;

    std::FILE* file_ = nullptr;
    off_t currentOffset_ = 0;
    bool isOpen_ = false;
};

}  // namespace multio::distGrib1ToGrib2::grib2grib
