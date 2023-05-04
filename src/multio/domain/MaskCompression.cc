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

#include "multio/util/BinaryUtils.h"

#include "eckit/io/Buffer.h"


namespace multio {
namespace domain {

//==============================================================================

MaskCompressionException::MaskCompressionException(const std::string& what, const eckit::CodeLocation& l) :
    eckit::Exception(std::string("MaskCompressionException: ") + what, l) {}


//==============================================================================

MaskPayloadHeader decodeMaskPayloadHeader(const unsigned char* b, std::size_t size) {
    if (size < MASK_PAYLOAD_HEADER_SIZE) {
        throw MaskCompressionException(
            std::string("Buffer needs to contain at least 5 bytes for compression header information"));
    };
    MaskPayloadHeader h;
    // 2 leftmost bits of the first byte are reserved for the format
    h.format = static_cast<MaskPayloadFormat>(b[0] >> 7);
    h.numBits = (static_cast<std::size_t>(b[1]) << 24) | (static_cast<std::size_t>(b[2]) << 16)
              | (static_cast<std::size_t>(b[3]) << 8) | static_cast<std::size_t>(b[4]);
    if (h.format == MaskPayloadFormat::RunLength) {
        // 6 Last bits of the first byte are used to store the number of byts per int (up to 64...)
        h.sparseNumBitsPerInt = ((b[0] & ((1UL << 6) - 1)) + 1);
        h.sparseStartValue = static_cast<bool>((b[0] >> 6) & 0x01);
    }
    else {
        h.sparseNumBitsPerInt = 0;
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
    std::array<unsigned char, MASK_PAYLOAD_HEADER_SIZE> b;
    b[0] = (static_cast<unsigned char>(h.format) << 7);
    if (h.format == MaskPayloadFormat::RunLength) {
        b[0] |= (h.sparseStartValue ? 0x01 : 0x00) << 6;
        if (h.sparseNumBitsPerInt > MASK_PAYLOAD_SPARSE_MAX_NUM_BITS) {
            throw MaskCompressionException(
                std::string("RunLength format must provide a number of bits <=64 for integer representation"));
        }
        b[0] |= ((h.sparseNumBitsPerInt - 1) & 0xFF);
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


//==============================================================================

std::size_t computeBufferSizeMaskBitMask(std::size_t size) {
    return MASK_PAYLOAD_HEADER_SIZE + (size >> 3) + ((size & ((1 << 3) - 1)) == 0 ? 0 : 1);
}

template <typename T>
MaskRunLengthProperties computeMaskRunLengthProperties(const T* maskVals, std::size_t size) noexcept {
    MaskRunLengthProperties p;
    p.bufSize = 0;
    p.numValues = 0;
    p.numBitsPerInt = 0;
    p.startValue = false;

    if (size == 0)
        return p;

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

    p.numBitsPerInt = multio::util::bitWidth(maxNum);
    std::size_t bufSizeBits = p.numBitsPerInt * p.numValues;
    // Round up to 8 and add header size
    p.bufSize = bufSizeBits / 8 + (((bufSizeBits & ((1 << 3) - 1)) == 0) ? 0 : 1) + MASK_PAYLOAD_HEADER_SIZE;

    return p;
}

template <typename T>
eckit::Buffer encodeMaskBitMask(const T* maskVals, const std::size_t size) {
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

template <typename T>
eckit::Buffer encodeMaskRunLength(const T* maskVals, const std::size_t size, const MaskRunLengthProperties props) {
    MaskPayloadHeader h;
    h.format = MaskPayloadFormat::RunLength;
    h.numBits = size;
    h.sparseNumBitsPerInt = props.numBitsPerInt;
    h.sparseStartValue = props.startValue;
    auto encodedHeader = encodeMaskPayloadHeader(h);

    eckit::Buffer b{props.bufSize * sizeof(uint8_t)};

    std::memcpy(b.data(), encodedHeader.data(), encodedHeader.size());

    std::size_t counter = 1;
    std::size_t bufOffset = MASK_PAYLOAD_HEADER_SIZE;
    std::size_t remainingBits = 8;
    bool currentSign = props.startValue;

    auto writeCounter = [&b, &counter, &remainingBits, &bufOffset, &props]() {
        std::size_t numBitsToWrite = props.numBitsPerInt;
        do {
            // Write bytes in big endian order
            if (remainingBits >= numBitsToWrite) {
                std::size_t nextRemainingBits = remainingBits - numBitsToWrite;
                b[bufOffset] |= ((counter & ((1UL << numBitsToWrite) - 1)) << nextRemainingBits);
                remainingBits = nextRemainingBits;
                numBitsToWrite = 0;
            }
            else {
                std::size_t nextNumBitsToWrite = numBitsToWrite - remainingBits;
                b[bufOffset] |= ((counter >> nextNumBitsToWrite) & ((1UL << remainingBits) - 1));
                numBitsToWrite = nextNumBitsToWrite;
                remainingBits = 0;
            }
            // Step to next byte
            if (remainingBits == 0) {
                ++bufOffset;
                b[bufOffset] = 0;
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

template <typename T>
eckit::Buffer encodeMaskRunLength(const T* maskVals, const std::size_t size) {
    return encodeMaskRunLength(maskVals, size, computeMaskRunLengthProperties(maskVals, size));
}

template eckit::Buffer encodeMaskRunLength<float>(const float* maskVals, std::size_t size);
template eckit::Buffer encodeMaskRunLength<double>(const double* maskVals, std::size_t size);
template eckit::Buffer encodeMaskRunLength<unsigned char>(const unsigned char* maskVals, std::size_t size);

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


// Explicitly instantiate ... will instantiate all other dependent functions and provide them for the test
template eckit::Buffer encodeMask<float>(const float* maskVals, std::size_t size);
template eckit::Buffer encodeMask<double>(const double* maskVals, std::size_t size);
template eckit::Buffer encodeMask<unsigned char>(const unsigned char* maskVals, std::size_t size);

//==============================================================================


// Iterator for decoding...
MaskPayloadIterator::MaskPayloadIterator(eckit::Buffer const& payload, MaskPayloadHeader header, bool checkConsistency,
                                         bool toEnd) :
    payload_(payload),
    header_(header),
    index_(0),
    sparseOffset_{MASK_PAYLOAD_HEADER_SIZE},
    sparseRemainingBits_{8},
    sparseNum_{0},
    sparseNumCounter_{0},
    val_{header_.sparseStartValue}  // Default value initialize value to false.
                                    // For sparse formatting this is important as the next updateValue_ call will toggle
                                    // the value and start with writing 1.
{
    // Check payload sizes...
    if (checkConsistency) {
        switch (header_.format) {
            case MaskPayloadFormat::RunLength: {
                // Payload is described by sequence of numbers that nominate the
                // numbers of consecutive 1 and 0 in alternating order
                // Hence all numbers must sum up to this size
                std::size_t countTotalBits = 0;

                const std::size_t NUM_BITS = header_.sparseNumBitsPerInt;

                // Parse bytes to num big endian order
                uint64_t num = 0;
                std::size_t remainingBits = 8;
                std::size_t numBitsToRead = NUM_BITS;
                std::size_t bufOffset = MASK_PAYLOAD_HEADER_SIZE;
                unsigned char b = payload_[bufOffset];
                for (;;) {
                    if (remainingBits >= numBitsToRead) {
                        std::size_t nextRemainingBits = remainingBits - numBitsToRead;
                        num |= (b & (((1 << numBitsToRead) - 1) << nextRemainingBits)) >> nextRemainingBits;

                        countTotalBits += num;

                        num = 0;

                        remainingBits = nextRemainingBits;
                        numBitsToRead = NUM_BITS;
                    }
                    else {
                        std::size_t nextNumBitsToRead = numBitsToRead - remainingBits;
                        num |= (b & ((1 << remainingBits) - 1)) << nextNumBitsToRead;

                        if (nextNumBitsToRead == 0) {
                            countTotalBits += num;

                            num = 0;
                            numBitsToRead = NUM_BITS;
                        }
                        else {
                            numBitsToRead = nextNumBitsToRead;
                        }
                        remainingBits = 0;
                    }
                    if (remainingBits == 0) {
                        ++bufOffset;
                        if (bufOffset >= payload_.size()) {
                            break;
                        }
                        b = payload_[bufOffset];
                        remainingBits = 8;
                    }
                }
                if (header_.numBits != countTotalBits) {
                    std::ostringstream oss;
                    oss << "RunLength mask compression unconsistent. Counted " << countTotalBits << " bits "
                        << " but the header specified " << header_.numBits;
                    throw MaskCompressionException(oss.str());
                }
            } break;
            case MaskPayloadFormat::BitMask:
            default: {
                std::size_t exptPayloadSize = MASK_PAYLOAD_HEADER_SIZE + (header_.numBits >> 3)
                                            + ((header_.numBits & ((1 << 3) - 1)) == 0 ? 0 : 1);
                if (!(exptPayloadSize == payload_.size())) {
                    std::ostringstream oss;
                    oss << "Payload size unexpected - size: " << payload_.size() << " expected: ceil("
                        << header_.numBits << " / 8) + " << MASK_PAYLOAD_HEADER_SIZE << " = " << exptPayloadSize;
                    throw MaskCompressionException(oss.str());
                }
            }
        }
    }

    if (toEnd) {
        index_ = header_.numBits - 1;
    }
    else {
        updateValue_();
    }
}
MaskPayloadIterator::MaskPayloadIterator(eckit::Buffer const& payload, bool checkConsistency) :
    MaskPayloadIterator(payload, decodeMaskPayloadHeader(payload), checkConsistency) {}

MaskPayloadIterator::MaskPayloadIterator(const MaskPayloadIterator& other) :
    payload_{other.payload_},
    header_{other.header_},
    index_{other.index_},
    sparseOffset_{other.sparseOffset_},
    sparseRemainingBits_{other.sparseRemainingBits_},
    sparseNumCounter_{other.sparseNumCounter_},
    val_{other.val_} {}

MaskPayloadIterator::MaskPayloadIterator(MaskPayloadIterator&& other) noexcept :
    payload_{other.payload_},
    header_{other.header_},
    index_{other.index_},
    sparseOffset_{other.sparseOffset_},
    sparseRemainingBits_{other.sparseRemainingBits_},
    sparseNum_{other.sparseNum_},
    sparseNumCounter_{other.sparseNumCounter_},
    val_{other.val_} {}

// MaskPayloadIterator& MaskPayloadIterator::operator=(const MaskPayloadIterator& other) {
//     payload_ = other.payload_;
//     header_ = other.header_;
//     index_ = other.index_;

//     sparseOffset_ = other.sparseOffset_;
//     sparseRemainingBits_ = other.sparseRemainingBits_;
//     sparseNum_ = other.sparseNum_;
//     sparseNumCounter_ = other.sparseNumCounter_;

//     val_ = other.val_;
//     return *this;
// }

// MaskPayloadIterator& MaskPayloadIterator::operator=(MaskPayloadIterator&& other) {
//     payload_ = other.payload_;
//     header_ = other.header_;
//     index_ = other.index_;

//     sparseOffset_ = other.sparseOffset_;
//     sparseRemainingBits_ = other.sparseRemainingBits_;
//     sparseNum_ = other.sparseNum_;
//     sparseNumCounter_ = other.sparseNumCounter_;

//     val_ = other.val_;
//     return *this;
// }

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
                ++sparseNumCounter_;
                if (sparseNumCounter_ >= sparseNum_) {
                    updateValue_();
                    sparseNumCounter_ = 0;
                    val_ = !val_;
                }
            }
        } break;
        case MaskPayloadFormat::BitMask:
        default: {
            if ((index_ + 1) < header_.numBits) {
                ++index_;
                updateValue_();
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


void MaskPayloadIterator::updateValue_() noexcept {
    switch (header_.format) {
        case MaskPayloadFormat::RunLength: {
            const std::size_t NUM_BITS = header_.sparseNumBitsPerInt;

            std::size_t numBitsToRead = NUM_BITS;
            unsigned char b = payload_[sparseOffset_];
            sparseNum_ = 0;
            do {
                if (sparseRemainingBits_ >= numBitsToRead) {
                    std::size_t nextRemainingBits = sparseRemainingBits_ - numBitsToRead;
                    sparseNum_ |= (b & (((1 << numBitsToRead) - 1) << nextRemainingBits)) >> nextRemainingBits;

                    sparseRemainingBits_ = nextRemainingBits;
                    numBitsToRead = 0;
                }
                else {
                    std::size_t nextNumBitsToRead = numBitsToRead - sparseRemainingBits_;
                    sparseNum_ |= (b & ((1 << sparseRemainingBits_) - 1)) << nextNumBitsToRead;
                    numBitsToRead = nextNumBitsToRead;
                    sparseRemainingBits_ = 0;
                }
                if (sparseRemainingBits_ == 0) {
                    ++sparseOffset_;
                    if (sparseOffset_ >= payload_.size()) {
                        break;
                    }
                    b = payload_[sparseOffset_];
                    sparseRemainingBits_ = 8;
                }
            } while (numBitsToRead > 0);
        } break;
        case MaskPayloadFormat::BitMask:
        default: {
            val_ = static_cast<bool>(
                (payload_[(index_ >> 3) + MASK_PAYLOAD_HEADER_SIZE] >> (index_ & ((1 << 3) - 1)) & 0x01));
        }
    }
}

//==============================================================================

}  // namespace domain
}  // namespace multio
