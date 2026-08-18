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

#include "multio/tools/grib2grib/handleGribBoundaries.h"

namespace multio::distGrib1ToGrib2::grib2grib {

namespace {

/// @brief RAII deleter for raw ecCodes handles created during offset probing.
struct CodesHandleDeleter {
    /// @brief Delete the wrapped ecCodes handle when present.
    /// @param handle Raw ecCodes handle to release.
    void operator()(codes_handle* handle) const noexcept {
        if (handle != nullptr) {
            codes_handle_delete(handle);
        }
    }
};

/// @brief Unique-pointer alias used for temporary raw ecCodes handles.
using RawCodesHandlePtr = std::unique_ptr<codes_handle, CodesHandleDeleter>;

/// @brief Re-throw `errno`-based file failures with file-specific context.
/// @param operation Name of the failing libc operation.
/// @param filename File path associated with the failure.
/// @throw std::runtime_error Always.
[[noreturn]] void throwSystemError(const std::string& operation, const std::string& filename) {
    throw std::runtime_error(operation + " failed for '" + filename + "': " + std::strerror(errno));
}

/// @brief Convert an unsigned offset into `off_t` after bounds checking.
/// @param offset Candidate byte offset.
/// @param filename File used only for contextual error messages.
/// @return `offset` converted to `off_t`.
/// @throw std::overflow_error If the offset cannot be represented as `off_t`.
off_t checkedOffset(std::uint64_t offset, const std::string& filename) {
    const auto maximum = static_cast<std::uint64_t>(std::numeric_limits<off_t>::max());

    if (offset > maximum) {
        throw std::overflow_error("Offset exceeds off_t for '" + filename + "'");
    }

    return static_cast<off_t>(offset);
}

/// @brief Compute `ceil(numerator / denominator)` for positive integer quantities.
/// @param numerator Dividend.
/// @param denominator Divisor, which must be non-zero.
/// @return Ceiling integer quotient.
/// @throw std::invalid_argument If `denominator == 0`.
std::uint64_t ceilDivide(std::uint64_t numerator, std::uint64_t denominator) {
    if (denominator == 0) {
        throw std::invalid_argument("division by zero");
    }

    return numerator / denominator + static_cast<std::uint64_t>(numerator % denominator != 0);
}

/// @brief Locate the next GRIB message offset at or after a given file position.
/// @param file Already-opened file handle scanned via ecCodes.
/// @param filename File path used only for contextual error messages.
/// @param searchOffset Initial byte offset from which scanning starts.
/// @return Next GRIB message offset, or `std::nullopt` when no further message exists.
/// @throw std::runtime_error If file seeks or ecCodes probing fail.
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
        throw std::runtime_error("codes_get_message_offset() failed for '" + filename
                                 + "': " + codes_get_error_message(error));
    }

    return messageOffset;
}

std::optional<CandidateMessage> findNextGribOffsetByCandidateBoundary(std::FILE* file, const std::string& filename,
                                                                     off_t searchOffset, off_t endOffset,
                                                                     off_t fileEndOffset) {
    return searchCandidateMessage(file, filename, searchOffset, endOffset, fileEndOffset);
}

std::unique_ptr<metkit::codes::CodesHandle> decodeMessageAtCandidateBoundary(std::FILE* file, const std::string& filename,
                                                                              const CandidateMessage& candidate) {
    if (candidate.length > static_cast<std::uint64_t>(std::numeric_limits<std::size_t>::max())) {
        throw std::runtime_error("Candidate message length exceeds size_t for '" + filename + "'");
    }

    std::vector<std::uint8_t> buffer(static_cast<std::size_t>(candidate.length));

    if (fseeko(file, candidate.offset, SEEK_SET) != 0) {
        throwSystemError("fseeko", filename);
    }

    const auto nread = std::fread(buffer.data(), 1, buffer.size(), file);
    if (nread != buffer.size()) {
        if (ferror(file)) {
            throwSystemError("fread", filename);
        }

        throw std::runtime_error("Short read while decoding candidate message for '" + filename + "'");
    }

    return metkit::codes::codesHandleFromMessageCopy(metkit::codes::Span<const std::uint8_t>(buffer.data(), buffer.size()));
}

/// @brief Append one unsigned 64-bit integer to a binary payload.
/// @param buffer Destination payload grown in little-endian byte order.
/// @param value Value to append.
void appendU64(std::vector<char>& buffer, std::uint64_t value) {
    for (unsigned shift = 0; shift < 64; shift += 8) {
        buffer.push_back(static_cast<char>((value >> shift) & 0xffu));
    }
}

/// @brief Read one unsigned 64-bit integer from a binary payload.
/// @param buffer Source payload encoded in little-endian byte order.
/// @param cursor In-out cursor advanced past the consumed bytes.
/// @return Decoded integer value.
/// @throw std::runtime_error If the payload is truncated.
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

/// @brief Append one length-prefixed string to a binary payload.
/// @param buffer Destination payload grown in-place.
/// @param value String value to encode.
void appendString(std::vector<char>& buffer, const std::string& value) {
    appendU64(buffer, static_cast<std::uint64_t>(value.size()));
    buffer.insert(buffer.end(), value.begin(), value.end());
}

/// @brief Read one length-prefixed string from a binary payload.
/// @param buffer Source payload.
/// @param cursor In-out cursor advanced past the consumed bytes.
/// @return Decoded string value.
/// @throw std::runtime_error If the payload is truncated.
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

/// @brief Query the size of one regular file in bytes.
/// @param path Absolute or relative path to the input file.
/// @return File size cast to `long` after validation.
/// @throw std::runtime_error If the path cannot be stat'ed, is not a regular file, or is too large.
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

/// @brief Split a file into at most `nChunks` contiguous byte ranges.
/// @param filename Input file to split.
/// @param nChunks Requested number of chunks before bounding by file size.
/// @return Half-open work-unit ranges covering the whole file.
/// @throw std::invalid_argument If `nChunks == 0`.
/// @throw std::runtime_error If the file size cannot be determined.
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

/// @brief Split a file into contiguous byte ranges not exceeding a reference size.
/// @param filename Input file to split.
/// @param maximumSizeBytes Maximum target size of each produced range.
/// @return Half-open work-unit ranges covering the whole file.
/// @throw std::invalid_argument If `maximumSizeBytes == 0`.
/// @throw std::runtime_error If the file size cannot be determined.
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

/// @brief Serialize one work-unit record into a compact binary payload.
/// @param workUnit Work-unit metadata to encode.
/// @return Binary payload suitable for MPI transfer or persistence.
std::vector<char> serializeWorkUnit(const WorkUnit& workUnit) {
    std::vector<char> payload;
    appendString(payload, workUnit.filename);
    appendU64(payload, static_cast<std::uint64_t>(workUnit.startOffset));
    appendU64(payload, static_cast<std::uint64_t>(workUnit.endOffset));
    return payload;
}

/// @brief Deserialize one work-unit record from a compact binary payload.
/// @param payload Binary payload previously produced by `serializeWorkUnit(...)`.
/// @return Reconstructed work-unit metadata.
/// @throw std::runtime_error If the payload is truncated or inconsistent.
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

/// @brief Serialize a sequence of work units into one compact binary payload.
/// @param workUnits Work-unit metadata sequence to encode.
/// @return Binary payload suitable for MPI transfer or persistence.
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

/// @brief Deserialize a sequence of work units from a compact binary payload.
/// @param payload Binary payload previously produced by `serializeWorkUnits(...)`.
/// @return Reconstructed work-unit sequence.
/// @throw std::runtime_error If the payload is truncated or inconsistent.
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

/// @brief Bind the reader to one immutable scheduled byte range.
/// @param workUnit Scheduled file slice whose messages will be iterated.
UnitOfWork::UnitOfWork(WorkUnit workUnit, WorkUnitReaderMode readerMode) :
    workUnit_{std::move(workUnit)}, readerMode_{readerMode} {}

/// @brief Close any open file handle on destruction.
UnitOfWork::~UnitOfWork() noexcept {
    close();
}

/// @brief Expose the immutable scheduling metadata associated with this reader.
/// @return Stored work-unit descriptor.
const WorkUnit& UnitOfWork::workUnit() const noexcept {
    return workUnit_;
}

/// @brief Report the raw byte span covered by the scheduled range.
/// @return `endOffset - startOffset` interpreted as an unsigned size.
std::uint64_t UnitOfWork::theoreticalSize() const noexcept {
    return static_cast<std::uint64_t>(workUnit_.endOffset - workUnit_.startOffset);
}

/// @brief Open the file and align reading to the first GRIB message inside the range.
/// @throw std::runtime_error If the file cannot be opened or GRIB scanning fails.
void UnitOfWork::open() {
    if (isOpen_) {
        throw std::runtime_error("UnitOfWork is already open for '" + workUnit_.filename + "'");
    }

    file_ = std::fopen(workUnit_.filename.c_str(), "rb");
    if (!file_) {
        throwSystemError("fopen", workUnit_.filename);
    }

    fileEndOffset_ = checkedOffset(static_cast<std::uint64_t>(fileSizeBytes(workUnit_.filename)), workUnit_.filename);

    // Work-unit offsets come from coarse byte-based scheduling, so align the
    // effective cursor to the first actual GRIB message starting inside range.
    if (readerMode_ == WorkUnitReaderMode::CandidateBoundary) {
        const auto firstCandidate =
            findNextGribOffsetByCandidateBoundary(file_, workUnit_.filename, workUnit_.startOffset, workUnit_.endOffset,
                                                  fileEndOffset_);
        if (!firstCandidate) {
            currentOffset_ = workUnit_.endOffset;
        }
        else {
            currentOffset_ = firstCandidate->offset;
        }
    }
    else {
        const auto firstMessageOffset = findNextGribOffset(file_, workUnit_.filename, workUnit_.startOffset);
        if (!firstMessageOffset || *firstMessageOffset >= workUnit_.endOffset) {
            currentOffset_ = workUnit_.endOffset;
        }
        else {
            currentOffset_ = *firstMessageOffset;
        }
    }

    if (currentOffset_ >= workUnit_.endOffset) {
        currentOffset_ = workUnit_.endOffset;
    }

    isOpen_ = true;
}

/// @brief Report whether another message may still be read from this range.
/// @return `true` when the file is open and the current cursor is still before `endOffset`.
bool UnitOfWork::newMessageAvailable() const noexcept {
    return isOpen_ && currentOffset_ < workUnit_.endOffset;
}

/// @brief Read and copy the next GRIB message inside the scheduled range.
/// @return Owning `CodesHandle` copy for the next message, or `nullptr` when the range is exhausted.
/// @throw std::runtime_error If file seeks, ecCodes decoding, or range checks fail.
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

    if (readerMode_ == WorkUnitReaderMode::CandidateBoundary) {
        const auto candidate = findNextGribOffsetByCandidateBoundary(file_, workUnit_.filename, currentOffset_,
                                                                     workUnit_.endOffset, fileEndOffset_);
        if (!candidate) {
            currentOffset_ = workUnit_.endOffset;
            return nullptr;
        }

        currentOffset_ = candidate->offset + static_cast<off_t>(candidate->length);
        return decodeMessageAtCandidateBoundary(file_, workUnit_.filename, *candidate);
    }

    int error = CODES_SUCCESS;
    RawCodesHandlePtr handle{codes_grib_handle_new_from_file(nullptr, file_, &error)};
    if (!handle) {
        if (error == CODES_SUCCESS || error == CODES_END_OF_FILE) {
            currentOffset_ = workUnit_.endOffset;
            return nullptr;
        }

        throw std::runtime_error("ecCodes failed while processing '" + workUnit_.filename + "' in ["
                                 + std::to_string(workUnit_.startOffset) + ", " + std::to_string(workUnit_.endOffset)
                                 + "): " + codes_get_error_message(error));
    }

    off_t messageOffset = 0;
    error = codes_get_message_offset(handle.get(), &messageOffset);
    if (error != CODES_SUCCESS) {
        throw std::runtime_error("codes_get_message_offset() failed for '" + workUnit_.filename
                                 + "': " + codes_get_error_message(error));
    }

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
        throw std::runtime_error("codes_get_message() failed for '" + workUnit_.filename
                                 + "': " + codes_get_error_message(error));
    }

    // ecCodes advances the file handle to the end of the decoded message, which
    // becomes the next candidate offset for this work-unit reader.
    const auto nextOffset = ftello(file_);
    if (nextOffset < 0) {
        throwSystemError("ftello", workUnit_.filename);
    }

    currentOffset_ = nextOffset;

    // Copy the message bytes out of the temporary ecCodes handle so the caller
    // owns an independent `CodesHandle` beyond the lifetime of this function.
    const auto* bytes = reinterpret_cast<const std::uint8_t*>(buffer);
    return metkit::codes::codesHandleFromMessageCopy(metkit::codes::Span<const std::uint8_t>(bytes, size));
}

/// @brief Close the underlying file handle and reset reader state.
/// @return `true` when close succeeded or no file was open, otherwise `false`.
bool UnitOfWork::close() noexcept {
    bool success = true;
    if (file_ != nullptr) {
        success = std::fclose(file_) == 0;
        file_ = nullptr;
    }
    fileEndOffset_ = 0;
    currentOffset_ = 0;
    isOpen_ = false;
    return success;
}

}  // namespace multio::distGrib1ToGrib2::grib2grib
