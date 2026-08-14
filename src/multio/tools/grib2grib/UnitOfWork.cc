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
/// @brief Work-unit abstraction implementation for chunk-based MPI orchestration in `grib2grib`.

#include "multio/tools/grib2grib/UnitOfWork.h"

#include <sys/stat.h>

#include <cerrno>
#include <cstring>
#include <limits>
#include <optional>
#include <stdexcept>

#include <eccodes.h>

#include "metkit/codes/api/CodesAPI.h"

namespace multio::distGrib1ToGrib2::grib2grib {

namespace {

struct CodesHandleDeleter {
    void operator()(codes_handle* handle) const noexcept {
        if (handle != nullptr) {
            codes_handle_delete(handle);
        }
    }
};

using RawCodesHandlePtr = std::unique_ptr<codes_handle, CodesHandleDeleter>;

[[noreturn]] void throwSystemError(const std::string& operation, const std::string& filename) {
    throw std::runtime_error(operation + " failed for '" + filename + "': " + std::strerror(errno));
}

off_t checkedOffset(std::uint64_t offset, const std::string& filename) {
    const auto maximum = static_cast<std::uint64_t>(std::numeric_limits<off_t>::max());

    if (offset > maximum) {
        throw std::overflow_error("Offset exceeds off_t for '" + filename + "'");
    }

    return static_cast<off_t>(offset);
}

std::uint64_t ceilDivide(std::uint64_t numerator, std::uint64_t denominator) {
    if (denominator == 0) {
        throw std::invalid_argument("division by zero");
    }

    return numerator / denominator + static_cast<std::uint64_t>(numerator % denominator != 0);
}

std::optional<off_t> findNextGribOffset(std::FILE* file, const std::string& filename, off_t searchOffset) {
    if (fseeko(file, searchOffset, SEEK_SET) != 0) {
        throwSystemError("fseeko", filename);
    }

    int error = CODES_SUCCESS;
    RawCodesHandlePtr handle{codes_grib_handle_new_from_file(nullptr, file, &error)};

    if (!handle) {
        if (error == CODES_SUCCESS || error == CODES_END_OF_FILE) {
            return std::nullopt;
        }

        throw std::runtime_error("ecCodes failed while scanning '" + filename + "' from offset "
                                 + std::to_string(searchOffset) + ": " + codes_get_error_message(error));
    }

    off_t messageOffset = 0;
    error = codes_get_message_offset(handle.get(), &messageOffset);
    if (error != CODES_SUCCESS) {
        throw std::runtime_error("codes_get_message_offset() failed for '" + filename + "': "
                                 + codes_get_error_message(error));
    }

    return messageOffset;
}

void appendU64(std::vector<char>& buffer, std::uint64_t value) {
    for (unsigned shift = 0; shift < 64; shift += 8) {
        buffer.push_back(static_cast<char>((value >> shift) & 0xffu));
    }
}

std::uint64_t readU64(const std::vector<char>& buffer, std::size_t& cursor) {
    if (buffer.size() - cursor < 8) {
        throw std::runtime_error("Truncated payload while reading uint64");
    }

    std::uint64_t value = 0;
    for (unsigned shift = 0; shift < 64; shift += 8) {
        value |= static_cast<std::uint64_t>(static_cast<unsigned char>(buffer[cursor++])) << shift;
    }
    return value;
}

void appendString(std::vector<char>& buffer, const std::string& value) {
    appendU64(buffer, static_cast<std::uint64_t>(value.size()));
    buffer.insert(buffer.end(), value.begin(), value.end());
}

std::string readString(const std::vector<char>& buffer, std::size_t& cursor) {
    const auto length = readU64(buffer, cursor);
    if (length > static_cast<std::uint64_t>(buffer.size() - cursor)) {
        throw std::runtime_error("Truncated payload while reading string");
    }

    const auto size = static_cast<std::size_t>(length);
    std::string value(buffer.data() + cursor, size);
    cursor += size;
    return value;
}

}  // namespace

long fileSizeBytes(const std::string& path) {
    struct stat st{};

    if (::stat(path.c_str(), &st) != 0) {
        throw std::runtime_error("stat failed for '" + path + "': " + std::strerror(errno));
    }

    if (!S_ISREG(st.st_mode)) {
        throw std::runtime_error("not a regular file: " + path);
    }

    if (st.st_size < 0) {
        throw std::runtime_error("negative file size reported for: " + path);
    }

    if (static_cast<unsigned long long>(st.st_size)
        > static_cast<unsigned long long>(std::numeric_limits<long>::max())) {
        throw std::runtime_error("file too large for long: " + path);
    }

    return static_cast<long>(st.st_size);
}

std::vector<WorkUnit> splitFileIntoNWorkUnits(const std::string& filename, std::size_t nChunks) {
    if (nChunks == 0) {
        throw std::invalid_argument("nChunks must be > 0");
    }

    const auto totalSize = static_cast<std::uint64_t>(fileSizeBytes(filename));
    if (totalSize == 0) {
        return {};
    }

    const auto boundedChunks = std::min<std::uint64_t>(nChunks, totalSize);
    std::vector<WorkUnit> workUnits;
    workUnits.reserve(static_cast<std::size_t>(boundedChunks));

    for (std::uint64_t i = 0; i < boundedChunks; ++i) {
        const auto start = (totalSize * i) / boundedChunks;
        const auto end = (totalSize * (i + 1)) / boundedChunks;
        if (start >= end) {
            continue;
        }

        workUnits.push_back(WorkUnit{filename, checkedOffset(start, filename), checkedOffset(end, filename)});
    }

    return workUnits;
}

std::vector<WorkUnit> splitFileByMaximumWorkUnitSize(const std::string& filename, std::uint64_t maximumSizeBytes) {
    if (maximumSizeBytes == 0) {
        throw std::invalid_argument("maximumSizeBytes must be > 0");
    }

    const auto totalSize = static_cast<std::uint64_t>(fileSizeBytes(filename));
    if (totalSize == 0) {
        return {};
    }

    return splitFileIntoNWorkUnits(filename, static_cast<std::size_t>(ceilDivide(totalSize, maximumSizeBytes)));
}

std::vector<char> serializeWorkUnit(const WorkUnit& workUnit) {
    std::vector<char> payload;
    appendString(payload, workUnit.filename);
    appendU64(payload, static_cast<std::uint64_t>(workUnit.startOffset));
    appendU64(payload, static_cast<std::uint64_t>(workUnit.endOffset));
    return payload;
}

WorkUnit deserializeWorkUnit(const std::vector<char>& payload) {
    std::size_t cursor = 0;

    WorkUnit workUnit;
    workUnit.filename = readString(payload, cursor);
    workUnit.startOffset = checkedOffset(readU64(payload, cursor), workUnit.filename);
    workUnit.endOffset = checkedOffset(readU64(payload, cursor), workUnit.filename);

    if (workUnit.startOffset > workUnit.endOffset) {
        throw std::runtime_error("Invalid WorkUnit: startOffset > endOffset for '" + workUnit.filename + "'");
    }

    if (cursor != payload.size()) {
        throw std::runtime_error("Unexpected trailing data while deserializing WorkUnit");
    }

    return workUnit;
}

std::vector<char> serializeWorkUnits(const std::vector<WorkUnit>& workUnits) {
    std::vector<char> payload;
    appendU64(payload, static_cast<std::uint64_t>(workUnits.size()));

    for (const auto& workUnit : workUnits) {
        appendString(payload, workUnit.filename);
        appendU64(payload, static_cast<std::uint64_t>(workUnit.startOffset));
        appendU64(payload, static_cast<std::uint64_t>(workUnit.endOffset));
    }

    return payload;
}

std::vector<WorkUnit> deserializeWorkUnits(const std::vector<char>& payload) {
    std::size_t cursor = 0;
    const auto count = readU64(payload, cursor);

    std::vector<WorkUnit> workUnits;
    workUnits.reserve(static_cast<std::size_t>(count));

    for (std::uint64_t i = 0; i < count; ++i) {
        WorkUnit workUnit;
        workUnit.filename = readString(payload, cursor);
        workUnit.startOffset = checkedOffset(readU64(payload, cursor), workUnit.filename);
        workUnit.endOffset = checkedOffset(readU64(payload, cursor), workUnit.filename);

        if (workUnit.startOffset > workUnit.endOffset) {
            throw std::runtime_error("Invalid WorkUnit: startOffset > endOffset for '" + workUnit.filename + "'");
        }

        workUnits.push_back(std::move(workUnit));
    }

    if (cursor != payload.size()) {
        throw std::runtime_error("Unexpected trailing data while deserializing WorkUnits");
    }

    return workUnits;
}

UnitOfWork::UnitOfWork(WorkUnit workUnit) : workUnit_{std::move(workUnit)} {}

UnitOfWork::~UnitOfWork() noexcept {
    close();
}

const WorkUnit& UnitOfWork::workUnit() const noexcept {
    return workUnit_;
}

std::uint64_t UnitOfWork::theoreticalSize() const noexcept {
    return static_cast<std::uint64_t>(workUnit_.endOffset - workUnit_.startOffset);
}

void UnitOfWork::open() {
    if (isOpen_) {
        throw std::runtime_error("UnitOfWork is already open for '" + workUnit_.filename + "'");
    }

    file_ = std::fopen(workUnit_.filename.c_str(), "rb");
    if (!file_) {
        throwSystemError("fopen", workUnit_.filename);
    }

    const auto firstMessageOffset = findNextGribOffset(file_, workUnit_.filename, workUnit_.startOffset);
    if (!firstMessageOffset || *firstMessageOffset >= workUnit_.endOffset) {
        currentOffset_ = workUnit_.endOffset;
    }
    else {
        currentOffset_ = *firstMessageOffset;
    }

    isOpen_ = true;
}

bool UnitOfWork::newMessageAvailable() const noexcept {
    return isOpen_ && currentOffset_ < workUnit_.endOffset;
}

std::unique_ptr<metkit::codes::CodesHandle> UnitOfWork::nextMessage() {
    if (!isOpen_) {
        throw std::runtime_error("UnitOfWork is not open for '" + workUnit_.filename + "'");
    }

    if (!newMessageAvailable()) {
        return nullptr;
    }

    if (fseeko(file_, currentOffset_, SEEK_SET) != 0) {
        throwSystemError("fseeko", workUnit_.filename);
    }

    int error = CODES_SUCCESS;
    RawCodesHandlePtr handle{codes_grib_handle_new_from_file(nullptr, file_, &error)};
    if (!handle) {
        if (error == CODES_SUCCESS || error == CODES_END_OF_FILE) {
            currentOffset_ = workUnit_.endOffset;
            return nullptr;
        }

        throw std::runtime_error("ecCodes failed while processing '" + workUnit_.filename + "' in ["
                                 + std::to_string(workUnit_.startOffset) + ", "
                                 + std::to_string(workUnit_.endOffset) + "): " + codes_get_error_message(error));
    }

    off_t messageOffset = 0;
    error = codes_get_message_offset(handle.get(), &messageOffset);
    if (error != CODES_SUCCESS) {
        throw std::runtime_error("codes_get_message_offset() failed for '" + workUnit_.filename + "': "
                                 + codes_get_error_message(error));
    }
    std::cout << "messageOffset: " << messageOffset << std::endl;

    if (messageOffset >= workUnit_.endOffset) {
        currentOffset_ = workUnit_.endOffset;
        return nullptr;
    }

    if (messageOffset < workUnit_.startOffset) {
        throw std::runtime_error("ecCodes returned a message before the work-unit start in '" + workUnit_.filename
                                 + "'");
    }

    const void* buffer = nullptr;
    size_t size = 0;
    error = codes_get_message(handle.get(), &buffer, &size);
    if (error != CODES_SUCCESS) {
        throw std::runtime_error("codes_get_message() failed for '" + workUnit_.filename + "': "
                                 + codes_get_error_message(error));
    }

    const auto nextOffset = ftello(file_);
    if (nextOffset < 0) {
        throwSystemError("ftello", workUnit_.filename);
    }

    currentOffset_ = nextOffset;

    const auto* bytes = reinterpret_cast<const std::uint8_t*>(buffer);
    return metkit::codes::codesHandleFromMessageCopy(metkit::codes::Span<const std::uint8_t>(bytes, size));
}

bool UnitOfWork::close() noexcept {
    bool success = true;
    if (file_ != nullptr) {
        success = std::fclose(file_) == 0;
        file_ = nullptr;
    }
    currentOffset_ = 0;
    isOpen_ = false;
    return success;
}

}  // namespace multio::distGrib1ToGrib2::grib2grib
