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
/// @brief Helpers for validating candidate GRIB message boundaries.

#include "multio/tools/grib2grib/handleGribBoundaries.h"

#include <cerrno>
#include <cstring>
#include <limits>
#include <stdexcept>
#include <vector>

#include <eccodes.h>

namespace multio::distGrib1ToGrib2::grib2grib {
namespace {

constexpr unsigned char gribMagic[4] = {'G', 'R', 'I', 'B'};
constexpr unsigned char gribEnd[4] = {'7', '7', '7', '7'};

[[noreturn]] void throwSystemError(const char* operation, const std::string& filename) {
    throw std::runtime_error(std::string(operation) + " failed for '" + filename + "': " + std::strerror(errno));
}

std::uint64_t readUint24BE(const unsigned char* p) {
    return (static_cast<std::uint64_t>(p[0]) << 16) | (static_cast<std::uint64_t>(p[1]) << 8)
         | static_cast<std::uint64_t>(p[2]);
}

std::uint64_t readUint64BE(const unsigned char* p) {
    return (static_cast<std::uint64_t>(p[0]) << 56) | (static_cast<std::uint64_t>(p[1]) << 48)
         | (static_cast<std::uint64_t>(p[2]) << 40) | (static_cast<std::uint64_t>(p[3]) << 32)
         | (static_cast<std::uint64_t>(p[4]) << 24) | (static_cast<std::uint64_t>(p[5]) << 16)
         | (static_cast<std::uint64_t>(p[6]) << 8) | static_cast<std::uint64_t>(p[7]);
}

std::optional<std::uint64_t> readLargeGrib1Length(std::FILE* file, const std::string& filename, off_t candidateOffset,
                                                  off_t fileEndOffset) {
    clearerr(file);
    if (fseeko(file, candidateOffset, SEEK_SET) != 0) {
        throwSystemError("fseeko", filename);
    }

    int error = CODES_SUCCESS;
    codes_handle* handle = codes_grib_handle_new_from_file(nullptr, file, &error);
    if (handle == nullptr) {
        return std::nullopt;
    }

    off_t messageOffset = 0;
    error = codes_get_message_offset(handle, &messageOffset);
    codes_handle_delete(handle);
    if (error != CODES_SUCCESS || messageOffset != candidateOffset) {
        return std::nullopt;
    }

    const off_t messageEndOffset = ftello(file);
    if (messageEndOffset <= candidateOffset || messageEndOffset > fileEndOffset) {
        return std::nullopt;
    }

    return static_cast<std::uint64_t>(messageEndOffset - candidateOffset);
}

bool readExactlyAt(std::FILE* file, const std::string& filename, off_t offset, void* out, std::size_t length) {
    clearerr(file);

    if (fseeko(file, offset, SEEK_SET) != 0) {
        throwSystemError("fseeko", filename);
    }

    if (length == 0) {
        return true;
    }

    const auto nread = std::fread(out, 1, length, file);
    if (nread == length) {
        return true;
    }

    if (ferror(file)) {
        throwSystemError("fread", filename);
    }

    clearerr(file);
    return false;
}

std::optional<CandidateMessage> tryValidateCandidate(std::FILE* file, const std::string& filename,
                                                     off_t candidateOffset, off_t fileEndOffset) {
    if (candidateOffset < 0 || candidateOffset >= fileEndOffset) {
        return std::nullopt;
    }

    const auto remaining = static_cast<std::uint64_t>(fileEndOffset - candidateOffset);
    if (remaining < 8) {
        return std::nullopt;
    }

    unsigned char header[16] = {};
    const std::size_t headerSize = remaining >= sizeof(header) ? sizeof(header) : static_cast<std::size_t>(remaining);

    if (!readExactlyAt(file, filename, candidateOffset, header, headerSize)) {
        return std::nullopt;
    }

    if (std::memcmp(header, gribMagic, sizeof(gribMagic)) != 0) {
        return std::nullopt;
    }

    const unsigned char edition = header[7];
    std::uint64_t messageLength = 0;

    switch (edition) {
        case 1:
            messageLength = readUint24BE(header + 4);
            // Bit 23 denotes the legacy large-GRIB1 representation. Its raw
            // header value is not necessarily the physical message length.
            if ((messageLength & 0x800000u) != 0) {
                const auto largeMessageLength = readLargeGrib1Length(file, filename, candidateOffset, fileEndOffset);
                if (!largeMessageLength) {
                    return std::nullopt;
                }
                messageLength = *largeMessageLength;
            }
            if (messageLength < 12) {
                return std::nullopt;
            }
            break;

        case 2:
            if (headerSize < sizeof(header)) {
                return std::nullopt;
            }
            messageLength = readUint64BE(header + 8);
            if (messageLength < 20) {
                return std::nullopt;
            }
            break;

        default:
            return std::nullopt;
    }

    if (messageLength > remaining) {
        return std::nullopt;
    }

    if (messageLength > static_cast<std::uint64_t>(std::numeric_limits<std::size_t>::max())) {
        return std::nullopt;
    }

    unsigned char trailer[4] = {};
    if (!readExactlyAt(file, filename, candidateOffset + static_cast<off_t>(messageLength) - 4, trailer,
                       sizeof(trailer))) {
        return std::nullopt;
    }

    if (std::memcmp(trailer, gribEnd, sizeof(gribEnd)) != 0) {
        return std::nullopt;
    }

    return CandidateMessage{candidateOffset, messageLength};
}

}  // namespace

std::optional<CandidateMessage> searchCandidateMessage(std::FILE* file, const std::string& filename, off_t searchOffset,
                                                       off_t endOffset, off_t fileEndOffset) {
    if (file == nullptr) {
        throw std::invalid_argument("searchCandidateMessage() received a null FILE*");
    }

    if (searchOffset < 0) {
        throw std::invalid_argument("searchCandidateMessage() requires searchOffset >= 0");
    }

    if (endOffset < searchOffset) {
        throw std::invalid_argument("searchCandidateMessage() requires endOffset >= searchOffset");
    }

    if (fileEndOffset < 0) {
        throw std::invalid_argument("searchCandidateMessage() requires fileEndOffset >= 0");
    }

    if (fileEndOffset < endOffset) {
        throw std::invalid_argument("searchCandidateMessage() requires fileEndOffset >= endOffset");
    }

    if (searchOffset == endOffset) {
        return std::nullopt;
    }

    std::vector<unsigned char> window(64 * 1024);
    off_t windowStart = searchOffset;

    while (windowStart < endOffset) {
        const off_t span = endOffset - windowStart;
        const std::size_t toRead
            = span >= static_cast<off_t>(window.size()) ? window.size() : static_cast<std::size_t>(span);

        clearerr(file);
        if (fseeko(file, windowStart, SEEK_SET) != 0) {
            throwSystemError("fseeko", filename);
        }

        const auto nread = std::fread(window.data(), 1, toRead, file);
        if (nread == 0) {
            if (ferror(file)) {
                throwSystemError("fread", filename);
            }
            break;
        }

        for (std::size_t i = 0; i + 4 <= nread; ++i) {
            if (std::memcmp(window.data() + i, gribMagic, sizeof(gribMagic)) != 0) {
                continue;
            }

            const off_t candidateOffset = windowStart + static_cast<off_t>(i);
            if (auto candidate = tryValidateCandidate(file, filename, candidateOffset, fileEndOffset)) {
                if (fseeko(file, candidate->offset, SEEK_SET) != 0) {
                    throwSystemError("fseeko", filename);
                }
                return candidate;
            }
        }

        if (windowStart + static_cast<off_t>(nread) >= endOffset) {
            break;
        }

        const off_t advance = nread > 3 ? static_cast<off_t>(nread - 3) : static_cast<off_t>(nread);
        windowStart += advance;
    }

    return std::nullopt;
}

}  // namespace multio::distGrib1ToGrib2::grib2grib
