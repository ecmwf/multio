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


#include "multio/message/SharedPayload.h"
#include "multio/util/BinaryUtils.h"
#include "multio/util/VariantHelpers.h"

#include "eckit/exception/Exceptions.h"
#include "eckit/io/Buffer.h"

#include <array>
#include <cstdint>
#include <cstring>  // memcpy

namespace eckit {
class Buffer;
}


namespace multio {
namespace domain {

//------------------------------------------------------------------------------

class MaskCompressionException : public eckit::Exception {
public:
    MaskCompressionException(const std::string& what, const eckit::CodeLocation& l = eckit::CodeLocation());
};


//------------------------------------------------------------------------------


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
    // For runLength representation Payload is described by sequence of numbers that nominate the
    // numbers of consecutive 1 and 0 in alternating order. Depending on the mask, these
    // numbers can be small or may be very large. In case of small numbers we avoid reserving
    // 4 or 8 bytes for one number and instead allow fewer bits...
    std::size_t runLengthNumBitsPerInt = 0;
    bool runLengthStartValue = 0;
};

static constexpr std::size_t MASK_PAYLOAD_HEADER_SIZE = 5;
static constexpr std::size_t MASK_PAYLOAD_SPARSE_MAX_NUM_BITS = 64;

MaskPayloadHeader decodeMaskPayloadHeader(const unsigned char* b, std::size_t size);
// MaskPayloadHeader decodeMaskPayloadHeader(const eckit::Buffer& b);
// MaskPayloadHeader decodeMaskPayloadHeader(const PayloadReference& b);
MaskPayloadHeader decodeMaskPayloadHeader(const message::SharedPayload& b);
MaskPayloadHeader decodeMaskPayloadHeader(const std::array<unsigned char, MASK_PAYLOAD_HEADER_SIZE>& b);

std::array<unsigned char, MASK_PAYLOAD_HEADER_SIZE> encodeMaskPayloadHeader(MaskPayloadHeader h);


//------------------------------------------------------------------------------

std::size_t computeBufferSizeMaskBitMask(std::size_t size);

struct MaskRunLengthProperties {
    std::size_t bufSize;
    std::size_t numValues;
    std::size_t numBitsPerInt;  // bufSize = numBitsPerInt * countValues + some roundup to 8...
    bool startValue;
};


template <typename Cont>
MaskRunLengthProperties computeMaskRunLengthProperties(const Cont& maskVals, std::size_t size) noexcept {
    MaskRunLengthProperties p;
    p.bufSize = 0;
    p.numValues = 0;
    p.numBitsPerInt = 0;
    p.startValue = false;

    if (size == 0) {
        return p;
    }

    p.startValue = static_cast<bool>(maskVals[0]);

    std::size_t maxNum = 0;

    std::size_t counter = 1;
    bool currentSign = p.startValue;
    for (unsigned int i = 1; i < size; ++i) {
        bool v = static_cast<bool>(maskVals[i]);
        if (v == currentSign) {
            ++counter;
        }
        else {
            ++p.numValues;
            maxNum = std::max(maxNum, counter);
            currentSign = v;
            counter = 1;
        }
    }
    ++p.numValues;
    maxNum = std::max(maxNum, counter);

    // Compute bitWidth for storing numbers with 1 offset - 0 have no meaning here, that's why it's possible to shift
    p.numBitsPerInt = multio::util::bitWidth(maxNum > 1 ? maxNum - 1 : 1);
    std::size_t bufSizeBits = p.numBitsPerInt * p.numValues;
    // Round up to 8 and add header size
    p.bufSize = (bufSizeBits >> 3) + (((bufSizeBits & ((1 << 3) - 1)) == 0) ? 0 : 1) + MASK_PAYLOAD_HEADER_SIZE;

    return p;
}


//------------------------------------------------------------------------------

template <typename Cont>
eckit::Buffer encodeMaskBitMask(const Cont& maskVals, const std::size_t size) {
    MaskPayloadHeader h;
    h.format = MaskPayloadFormat::BitMask;
    h.numBits = size;
    auto encodedHeader = encodeMaskPayloadHeader(h);

    eckit::Buffer b{computeBufferSizeMaskBitMask(size) * sizeof(uint8_t)};

    std::memcpy(b.data(), encodedHeader.data(), encodedHeader.size());

    for (unsigned int i = 0; i < size; ++i) {
        std::size_t offset = MASK_PAYLOAD_HEADER_SIZE + (i >> 3);
        std::size_t r8 = i & ((1 << 3) - 1);

        // Initialize
        if (r8 == 0) {
            b[offset] = 0;
        }

        if (static_cast<bool>(maskVals[i])) {
            b[offset] |= (0x01 << r8);
        }
    }

    return b;
}


//------------------------------------------------------------------------------

template <typename Cont>
eckit::Buffer encodeMaskRunLength(const Cont& maskVals, const std::size_t size, const MaskRunLengthProperties props) {
    MaskPayloadHeader h;
    h.format = MaskPayloadFormat::RunLength;
    h.numBits = size;
    h.runLengthNumBitsPerInt = props.numBitsPerInt;
    h.runLengthStartValue = props.startValue;
    auto encodedHeader = encodeMaskPayloadHeader(h);

    eckit::Buffer b{props.bufSize * sizeof(uint8_t)};

    std::memcpy(b.data(), encodedHeader.data(), encodedHeader.size());

    std::size_t counter = 1;
    std::size_t bufOffset = MASK_PAYLOAD_HEADER_SIZE;
    b[bufOffset] = 0;
    std::size_t remainingBits = 8;
    bool currentSign = props.startValue;

    auto writeCounter = [&b, &counter, &remainingBits, &bufOffset, &props]() {
        std::size_t numBitsToWrite = props.numBitsPerInt;
        std::size_t valToWrite = counter - 1;  // 0 Have no meaning, thats why we reduce and reincrement on decoding
        do {
            // Write bytes in big endian order
            if (remainingBits >= numBitsToWrite) {
                std::size_t nextRemainingBits = remainingBits - numBitsToWrite;
                b[bufOffset] |= ((valToWrite & ((1UL << numBitsToWrite) - 1)) << nextRemainingBits);
                remainingBits = nextRemainingBits;
                numBitsToWrite = 0;
            }
            else {
                std::size_t nextNumBitsToWrite = numBitsToWrite - remainingBits;
                b[bufOffset] |= ((valToWrite >> nextNumBitsToWrite) & ((1UL << remainingBits) - 1));
                numBitsToWrite = nextNumBitsToWrite;
                remainingBits = 0;
            }
            // Step to next byte
            if (remainingBits == 0) {
                ++bufOffset;
                if (bufOffset < props.bufSize) {
                    b[bufOffset] = 0;
                }
                else {
                    ASSERT(numBitsToWrite == 0);
                }
                remainingBits = 8;
            }
        } while (numBitsToWrite > 0);
    };

    for (unsigned int i = 1; i < size; ++i) {
        bool v = static_cast<bool>(maskVals[i]);
        if (v == currentSign) {
            ++counter;
        }
        else {
            // Now counter contains the number of iterest
            writeCounter();
            currentSign = v;
            counter = 1;
        }
    }

    // Write for the last time...
    writeCounter();

    return b;
}


template <typename Cont>
eckit::Buffer encodeMaskRunLength(const Cont& maskVals, const std::size_t size) {
    return encodeMaskRunLength(maskVals, size, computeMaskRunLengthProperties(maskVals, size));
}


//------------------------------------------------------------------------------

template <typename T>
eckit::Buffer encodeMask(const T* maskVals, const std::size_t size, const MaskRunLengthProperties props) {
    if (computeBufferSizeMaskBitMask(size) < props.bufSize) {
        return encodeMaskBitMask(maskVals, size);
    }
    else {
        return encodeMaskRunLength(maskVals, size, std::move(props));
    }
}
template <typename T>
eckit::Buffer encodeMask(const T* maskVals, const std::size_t size) {
    return encodeMask(maskVals, size, computeMaskRunLengthProperties(maskVals, size));
}


//------------------------------------------------------------------------------

// Iterator for decoding...
class RunLengthIterator {
public:
    using This = RunLengthIterator;

    using iterator_category = std::forward_iterator_tag;
    using difference_type = std::ptrdiff_t;
    using value_type = std::pair<bool, std::size_t>;
    using pointer = const std::pair<bool, std::size_t>*;
    using reference = const std::pair<bool, std::size_t>&;

    RunLengthIterator(message::PayloadReference const& payload, MaskPayloadHeader header, bool toEnd = false);
    RunLengthIterator(message::PayloadReference const& payload);

    RunLengthIterator(const This& other) = default;
    RunLengthIterator(This&& other) noexcept = default;

    RunLengthIterator& operator=(const RunLengthIterator&) = default;
    RunLengthIterator& operator=(RunLengthIterator&&) = default;

    reference operator*() const;
    reference operator*();

    pointer operator->() const;
    pointer operator->();

    This& operator++();

    This operator++(int);

    bool operator==(const This& other) const noexcept;

    bool operator!=(const This& other) const noexcept;

private:
    message::PayloadReference payload_;
    MaskPayloadHeader header_;
    std::size_t index_;                   // Global index of the bit
    std::size_t runLengthOffset_;         // runLength only: offset of the payload...
    std::size_t runLengthRemainingBits_;  // runLength only

    std::pair<bool, std::size_t> val_;

    void updateValue() noexcept;
};


class MaskPayloadIterator {
public:
    using This = MaskPayloadIterator;

    using iterator_category = std::forward_iterator_tag;
    using difference_type = std::ptrdiff_t;
    using value_type = bool;
    using pointer = const bool*;
    using reference = const bool&;

    MaskPayloadIterator(message::PayloadReference const& payload, MaskPayloadHeader header, bool toEnd = false);
    MaskPayloadIterator(message::PayloadReference const& payload);

    MaskPayloadIterator(const This& other) = default;
    MaskPayloadIterator(This&& other) noexcept = default;

    MaskPayloadIterator& operator=(const MaskPayloadIterator&) = default;
    MaskPayloadIterator& operator=(MaskPayloadIterator&&) = default;

    reference operator*() const;
    reference operator*();

    pointer operator->() const;
    pointer operator->();

    This& operator++();

    This operator++(int);

    bool operator==(const This& other) const noexcept;

    bool operator!=(const This& other) const noexcept;

private:
    message::PayloadReference payload_;
    MaskPayloadHeader header_;
    std::size_t index_;  // Global index of the bit
    bool val_;


    struct RL {
        RunLengthIterator it;
        uint64_t counter;  // runLength only: count up to runLengthNum_ - then the next offset and runLengthNum
                           // is evaluated. Val_ then toggles
    };

    std::optional<RL> rl_;

    void updateValue() noexcept;
};


//------------------------------------------------------------------------------

//  General container it access with a bitmask iterator
class EncodedBitMaskPayload {
public:
    EncodedBitMaskPayload(const message::PayloadReference& pr, const MaskPayloadHeader& header) :
        payload_(pr), header_(header) {}
    EncodedBitMaskPayload(const message::PayloadReference& pr) :
        EncodedBitMaskPayload(pr, decodeMaskPayloadHeader(pr)) {}
    EncodedBitMaskPayload(const eckit::Buffer& buf) :
        payload_(message::PayloadReference{buf.data(), buf.size()}), header_(decodeMaskPayloadHeader(payload_)) {}

    MaskPayloadIterator begin() const { return MaskPayloadIterator(payload_, header_); }

    MaskPayloadIterator cbegin() const { return MaskPayloadIterator(payload_, header_); }

    MaskPayloadIterator end() const { return MaskPayloadIterator(payload_, header_, true); }

    MaskPayloadIterator cend() const { return MaskPayloadIterator(payload_, header_, true); }

    std::size_t size() const noexcept { return header_.numBits; }

private:
    message::PayloadReference payload_;
    MaskPayloadHeader header_;
};


// Specialized container only applicable for run length encoded payload
class EncodedRunLengthPayload {
public:
    EncodedRunLengthPayload(const message::PayloadReference& pr, const MaskPayloadHeader& header) :
        payload_(pr), header_(header) {}
    EncodedRunLengthPayload(const message::PayloadReference& pr) :
        EncodedRunLengthPayload(pr, decodeMaskPayloadHeader(pr)) {}
    EncodedRunLengthPayload(const eckit::Buffer& buf) :
        payload_(message::PayloadReference{buf.data(), buf.size()}), header_(decodeMaskPayloadHeader(payload_)) {}

    RunLengthIterator begin() const { return RunLengthIterator(payload_, header_); }

    RunLengthIterator cbegin() const { return RunLengthIterator(payload_, header_); }

    RunLengthIterator end() const { return RunLengthIterator(payload_, header_, true); }

    RunLengthIterator cend() const { return RunLengthIterator(payload_, header_, true); }

    std::size_t size() const noexcept { return header_.numBits; }

private:
    message::PayloadReference payload_;
    MaskPayloadHeader header_;
};


using EncodedMaskPayload = std::variant<EncodedBitMaskPayload, EncodedRunLengthPayload>;

EncodedMaskPayload getEncodedMaskPayload(const message::PayloadReference& pr, const MaskPayloadHeader& header);
EncodedMaskPayload getEncodedMaskPayload(const message::PayloadReference& pr);
EncodedMaskPayload getEncodedMaskPayload(const eckit::Buffer& buf);


//------------------------------------------------------------------------------

}  // namespace domain
}  // namespace multio
