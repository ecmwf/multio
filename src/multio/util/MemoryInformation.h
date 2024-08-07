/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Razvan Aguridan

/// @date Jun 2024

#pragma once

#include <cstdint>
#include <map>
#include <vector>

namespace multio::util {

enum class InformationTypes {
    PeakVirtualMemory,
    VirtualMemory,
    LockedVirtualMemory,
    PinnedVirtualMemory,
    MaximumResidentMemory,
    ResidentMemory,
    AnonimousResidentMemory,
    FileMappingResidentMemory,
    SharedResidentMemory,
    DataVirtualMemory,
    StackVirtualMemory,
    TextSegmentVirtualMemory,
    SharedLibraryTextVirtualMemory,
    PageTableEntryVirtualMemory,
    SecondLevelPageTableEntryVirtualMemory,
    SwappedOutVirtualMemory,
    HugeTablesMemory
};

enum class InformationSizeUnits {
    Bytes,
    KiloBytes,
    MegaBytes,
    GigaBytes
};

struct InformationItem {
    uint64_t Value;
    InformationSizeUnits Unit;
};

class MemoryInformation {
public:
    MemoryInformation();
    ~MemoryInformation() = default;

    std::vector<InformationTypes> getAvailableKeys() const;

    const InformationItem& get(InformationTypes info) const { return memInfo_.at(info); }

private:
    MemoryInformation(MemoryInformation const&) = delete;
    MemoryInformation& operator=(MemoryInformation const&) = delete;

    MemoryInformation(MemoryInformation const&&) = delete;
    MemoryInformation& operator=(MemoryInformation const&&) = delete;

    const std::map<InformationTypes, InformationItem> memInfo_;
};

} // namespace multio::util