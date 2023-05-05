/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/// @author Philipp Geier

/// @date May 2023
#pragma once


#include "eckit/exception/Exceptions.h"

#include <array>
#include <cstdint>

namespace eckit {
class Buffer;
}


namespace multio {
namespace domain {

//==============================================================================

class MaskCompressionException : public eckit::Exception {
public:
    MaskCompressionException(const std::string& what, const eckit::CodeLocation& l = eckit::CodeLocation());
};


//==============================================================================


enum class MaskPayloadFormat : unsigned char
{
    BitMask = 0,    // Send as bitmask - putting 8 masks in one byte...
    RunLength = 1,  // Send as list of numbers that nominate the number of consecutive 1 and 0 in alternating order
                    // In the header the information of the start value (i.e. if the sequence starts with 1 or 0) is
                    // stored. E.g: Mask                            -> Numbers
                    //      1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0 -> 4 4 4 4
                    //      1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 -> 16
                    //      0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 -> 2 14
};


struct MaskPayloadHeader {
    MaskPayloadFormat format;
    std::size_t numBits;
    std::size_t runLengthNumBitsPerInt
        = 0;  // For runLength representation Payload is described by sequence of numbers that nominate the
              // numbers of consecutive 1 and 0 in alternating order. Depending on the mask, these
              // numbers can be small or may be very large. In case of small numbers we avoid reserving
              // 4 or 8 bytes for one number and instead allow less bits...
    bool runLengthStartValue = 0;
};

static constexpr std::size_t MASK_PAYLOAD_HEADER_SIZE = 5;
static constexpr std::size_t MASK_PAYLOAD_SPARSE_MAX_NUM_BITS = 64;

MaskPayloadHeader decodeMaskPayloadHeader(const unsigned char* b, std::size_t size);
MaskPayloadHeader decodeMaskPayloadHeader(const eckit::Buffer& b);
MaskPayloadHeader decodeMaskPayloadHeader(const std::array<unsigned char, MASK_PAYLOAD_HEADER_SIZE>& b);

std::array<unsigned char, MASK_PAYLOAD_HEADER_SIZE> encodeMaskPayloadHeader(MaskPayloadHeader h);


//==============================================================================

std::size_t computeBufferSizeMaskBitMask(std::size_t size);

struct MaskRunLengthProperties {
    std::size_t bufSize;
    std::size_t numValues;
    std::size_t numBitsPerInt;  // bufSize = numBitsPerInt * countValues + some roundup to 8...
    bool startValue;
};

template <typename T>
MaskRunLengthProperties computeMaskRunLengthProperties(const T* maskVals, std::size_t size) noexcept;

template <typename T>
eckit::Buffer encodeMaskBitMask(const T* maskVals, const std::size_t size);

template <typename T>
eckit::Buffer encodeMaskRunLength(const T* maskVals, const std::size_t size);
template <typename T>
eckit::Buffer encodeMaskRunLength(const T* maskVals, const std::size_t size, const MaskRunLengthProperties props);

template <typename T>
eckit::Buffer encodeMask(const T* maskVals, const std::size_t size);
template <typename T>
eckit::Buffer encodeMask(const T* maskVals, const std::size_t size, const MaskRunLengthProperties props);


//==============================================================================

// Iterator for decoding...
class MaskPayloadIterator {
public:
    using This = MaskPayloadIterator;

    using iterator_category = std::forward_iterator_tag;
    using difference_type = std::ptrdiff_t;
    using value_type = bool;
    using pointer = const bool*;
    using reference = const bool&;

    MaskPayloadIterator(eckit::Buffer const& payload, MaskPayloadHeader header, bool toEnd = false);
    MaskPayloadIterator(eckit::Buffer const& payload);

    MaskPayloadIterator(const This& other);
    MaskPayloadIterator(This&& other) noexcept;

    reference operator*() const;
    reference operator*();

    pointer operator->() const;
    pointer operator->();

    This& operator++();

    This operator++(int);

    bool operator==(const This& other) const noexcept;

    bool operator!=(const This& other) const noexcept;

private:
    eckit::Buffer const& payload_;
    MaskPayloadHeader header_;
    std::size_t index_;                   // Global index of the bit
    std::size_t runLengthOffset_;         // runLength only: offset of the payload...
    std::size_t runLengthRemainingBits_;  // runLength only

    uint64_t runLengthNum_;         // runLength only: decoded number of consecutive 0 or 1
    uint64_t runLengthNumCounter_;  // runLength only: count up to runLengthNum_ - then the next offset and runLengthNum
                                    // is evaluated. Val_ then toggles
    bool val_;

    void updateValue_() noexcept;
};


//==============================================================================

class EncodedMaskPayload {
public:
    EncodedMaskPayload(eckit::Buffer const& payload) : payload_(payload), header_(decodeMaskPayloadHeader(payload_)) {}

    MaskPayloadIterator begin() const { return MaskPayloadIterator(payload_, header_); }

    MaskPayloadIterator cbegin() const { return MaskPayloadIterator(payload_, header_); }

    MaskPayloadIterator end() const { return MaskPayloadIterator(payload_, header_, true); }

    MaskPayloadIterator cend() const { return MaskPayloadIterator(payload_, header_, true); }

    std::size_t size() const noexcept { return header_.numBits; }

private:
    const eckit::Buffer& payload_;
    MaskPayloadHeader header_;
};


//==============================================================================

}  // namespace domain
}  // namespace multio
