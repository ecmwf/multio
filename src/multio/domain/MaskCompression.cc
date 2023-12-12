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

#include "MaskCompression.h"


namespace multio {
namespace domain {

//------------------------------------------------------------------------------

MaskCompressionException::MaskCompressionException(const std::string& what, const eckit::CodeLocation& l) :
    eckit::Exception(std::string("MaskCompressionException: ") + what, l) {}


//------------------------------------------------------------------------------

MaskPayloadHeader decodeMaskPayloadHeader(const unsigned char* b, std::size_t size) {
    if (size < MASK_PAYLOAD_HEADER_SIZE) {
        throw MaskCompressionException(
            std::string("Buffer needs to contain at least 5 bytes for compression header information"));
    };
    MaskPayloadHeader h;
    // The leftmost two bits of the first byte are reserved for the format
    h.format = static_cast<MaskPayloadFormat>(b[0] >> 7);
    h.numBits = (static_cast<std::size_t>(b[1]) << 24) | (static_cast<std::size_t>(b[2]) << 16)
              | (static_cast<std::size_t>(b[3]) << 8) | static_cast<std::size_t>(b[4]);
    if (h.format == MaskPayloadFormat::RunLength) {
        // The last six bits of the first byte are used to store the number of byts per int (up to 64...)
        h.runLengthNumBitsPerInt = ((b[0] & ((1UL << 6) - 1)) + 1);
        h.runLengthStartValue = static_cast<bool>((b[0] >> 6) & 0x01);
    }
    else {
        h.runLengthNumBitsPerInt = 0;
    }

    return h;
}

MaskPayloadHeader decodeMaskPayloadHeader(const eckit::Buffer& b) {
    return decodeMaskPayloadHeader(static_cast<const unsigned char*>(b.data()), b.size() * sizeof(unsigned char));
}

MaskPayloadHeader decodeMaskPayloadHeader(const std::array<unsigned char, MASK_PAYLOAD_HEADER_SIZE>& b) {
    return decodeMaskPayloadHeader(b.data(), b.size());
}


std::array<unsigned char, MASK_PAYLOAD_HEADER_SIZE> encodeMaskPayloadHeader(MaskPayloadHeader h) {
    static_assert(CHAR_BIT == 8, "code is written for platforms with a char width of 8 bits");

    std::array<unsigned char, MASK_PAYLOAD_HEADER_SIZE> b;
    b[0] = (static_cast<unsigned char>(h.format) << 7);
    if (h.format == MaskPayloadFormat::RunLength) {
        b[0] |= (h.runLengthStartValue ? 0x01 : 0x00) << 6;
        if (h.runLengthNumBitsPerInt > MASK_PAYLOAD_SPARSE_MAX_NUM_BITS) {
            throw MaskCompressionException(
                std::string("RunLength format must provide a number of bits <=64 for integer representation"));
        }
        b[0] |= ((h.runLengthNumBitsPerInt - 1) & 0xFF);
    }
    if (h.numBits > ((1UL << 32) - 1)) {
        throw MaskCompressionException(
            std::string("Number of bits is larger than expected (greater than 1^32-1).  Please refactor the header to "
                        "reserve more bytes for that information."));
    }
    b[1] = (h.numBits >> 24) & 0xFF;
    b[2] = (h.numBits >> 16) & 0xFF;
    b[3] = (h.numBits >> 8) & 0xFF;
    b[4] = (h.numBits >> 0) & 0xFF;
    return b;
}


//------------------------------------------------------------------------------

std::size_t computeBufferSizeMaskBitMask(std::size_t size) {
    return MASK_PAYLOAD_HEADER_SIZE + (size >> 3) + ((size & ((1 << 3) - 1)) == 0 ? 0 : 1);
}


//------------------------------------------------------------------------------


// Iterator for decoding...
MaskPayloadIterator::MaskPayloadIterator(eckit::Buffer const& payload, MaskPayloadHeader header, bool toEnd) :
    payload_(payload),
    header_(header),
    index_(0),
    runLengthOffset_{MASK_PAYLOAD_HEADER_SIZE},
    runLengthRemainingBits_{8},
    runLengthNum_{0},
    runLengthNumCounter_{0},
    val_{header_.runLengthStartValue} {
    if (toEnd) {
        index_ = header_.numBits - 1;
    }
    else {
        updateValue();
    }
}

MaskPayloadIterator::MaskPayloadIterator(eckit::Buffer const& payload) :
    MaskPayloadIterator(payload, decodeMaskPayloadHeader(payload)) {}

MaskPayloadIterator::MaskPayloadIterator(const MaskPayloadIterator& other) :
    payload_{other.payload_},
    header_{other.header_},
    index_{other.index_},
    runLengthOffset_{other.runLengthOffset_},
    runLengthRemainingBits_{other.runLengthRemainingBits_},
    runLengthNumCounter_{other.runLengthNumCounter_},
    val_{other.val_} {}

MaskPayloadIterator::MaskPayloadIterator(MaskPayloadIterator&& other) noexcept :
    payload_{other.payload_},
    header_{other.header_},
    index_{other.index_},
    runLengthOffset_{other.runLengthOffset_},
    runLengthRemainingBits_{other.runLengthRemainingBits_},
    runLengthNum_{other.runLengthNum_},
    runLengthNumCounter_{other.runLengthNumCounter_},
    val_{other.val_} {}

MaskPayloadIterator::reference MaskPayloadIterator::operator*() const {
    return val_;
}

MaskPayloadIterator::reference MaskPayloadIterator::operator*() {
    return val_;
}

MaskPayloadIterator::pointer MaskPayloadIterator::operator->() const {
    return &val_;
}

MaskPayloadIterator::pointer MaskPayloadIterator::operator->() {
    return &val_;
}

MaskPayloadIterator& MaskPayloadIterator::operator++() {
    switch (header_.format) {
        case MaskPayloadFormat::RunLength: {
            if ((index_ + 1) < header_.numBits) {
                ++index_;
                ++runLengthNumCounter_;
                if (runLengthNumCounter_ >= runLengthNum_) {
                    updateValue();
                    runLengthNumCounter_ = 0;
                    val_ = !val_;
                }
            }
        } break;

        case MaskPayloadFormat::BitMask: {
            if ((index_ + 1) < header_.numBits) {
                ++index_;
                updateValue();
            }
        }
    }
    return *this;
}

MaskPayloadIterator MaskPayloadIterator::operator++(int) {
    MaskPayloadIterator current(*this);
    ++(*this);
    return current;
}

bool MaskPayloadIterator::operator==(const MaskPayloadIterator& other) const noexcept {
    return (&payload_ == &other.payload_) && (index_ == other.index_);
}

bool MaskPayloadIterator::operator!=(const MaskPayloadIterator& other) const noexcept {
    return (&payload_ != &other.payload_) || (index_ != other.index_);
}


void MaskPayloadIterator::updateValue() noexcept {
    switch (header_.format) {
        case MaskPayloadFormat::RunLength: {
            const std::size_t NUM_BITS = header_.runLengthNumBitsPerInt;

            std::size_t numBitsToRead = NUM_BITS;
            unsigned char b = payload_[runLengthOffset_];
            runLengthNum_ = 0;
            do {
                if (runLengthRemainingBits_ >= numBitsToRead) {
                    std::size_t nextRemainingBits = runLengthRemainingBits_ - numBitsToRead;
                    runLengthNum_ |= (b & (((1 << numBitsToRead) - 1) << nextRemainingBits)) >> nextRemainingBits;

                    runLengthRemainingBits_ = nextRemainingBits;
                    numBitsToRead = 0;
                }
                else {
                    std::size_t nextNumBitsToRead = numBitsToRead - runLengthRemainingBits_;
                    runLengthNum_ |= (b & ((1 << runLengthRemainingBits_) - 1)) << nextNumBitsToRead;
                    numBitsToRead = nextNumBitsToRead;
                    runLengthRemainingBits_ = 0;
                }
                if (runLengthRemainingBits_ == 0) {
                    ++runLengthOffset_;
                    if (runLengthOffset_ >= payload_.size()) {
                        break;
                    }
                    b = payload_[runLengthOffset_];
                    runLengthRemainingBits_ = 8;
                }
            } while (numBitsToRead > 0);

            // Eventually increase number by one as the encoding step decremnets by 1
            runLengthNum_ += 1;
        } break;

        case MaskPayloadFormat::BitMask: {
            val_ = static_cast<bool>(
                (payload_[(index_ >> 3) + MASK_PAYLOAD_HEADER_SIZE] >> (index_ & ((1 << 3) - 1)) & 0x01));
        }
    }
}

//------------------------------------------------------------------------------

}  // namespace domain
}  // namespace multio
