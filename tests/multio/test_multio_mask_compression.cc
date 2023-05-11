/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


/// @author Philipp Geier

#include <iostream>

#include "eckit/io/Buffer.h"
#include "eckit/testing/Test.h"

#include "multio/domain/MaskCompression.h"

#include <iomanip>

namespace multio {

namespace test {

using multio::domain::computeBufferSizeMaskBitMask;
using multio::domain::computeMaskRunLengthProperties;
using multio::domain::decodeMaskPayloadHeader;
using multio::domain::EncodedMaskPayload;
using multio::domain::encodeMask;
using multio::domain::encodeMaskBitMask;
using multio::domain::encodeMaskPayloadHeader;
using multio::domain::encodeMaskRunLength;
using multio::domain::MaskCompressionException;
using multio::domain::MaskPayloadFormat;
using multio::domain::MaskPayloadHeader;
using multio::domain::MaskPayloadIterator;
using multio::domain::MaskRunLengthProperties;

bool equalsMaskPayloadHeader(const MaskPayloadHeader& lhs, const MaskPayloadHeader& rhs) {
    return (lhs.format == rhs.format) && (lhs.numBits == rhs.numBits)
        && (lhs.runLengthNumBitsPerInt == rhs.runLengthNumBitsPerInt)
        && (lhs.runLengthStartValue == rhs.runLengthStartValue);
}

CASE("Encode/Decode MaskPayloadHeader") {
    MaskPayloadHeader h1;
    h1.format = domain::MaskPayloadFormat::BitMask;
    h1.numBits = 64;
    h1.runLengthNumBitsPerInt = 0;
    h1.runLengthStartValue = false;
    EXPECT(equalsMaskPayloadHeader(h1, decodeMaskPayloadHeader(encodeMaskPayloadHeader(h1))));

    MaskPayloadHeader h2;
    h2.format = domain::MaskPayloadFormat::RunLength;
    h2.numBits = 64;
    h2.runLengthNumBitsPerInt = 5;
    h2.runLengthStartValue = true;
    EXPECT(equalsMaskPayloadHeader(h2, decodeMaskPayloadHeader(encodeMaskPayloadHeader(h2))));

    MaskPayloadHeader h3;
    h3.format = domain::MaskPayloadFormat::RunLength;
    h3.numBits = 64;
    h3.runLengthNumBitsPerInt = 65;
    h3.runLengthStartValue = false;
    EXPECT_THROWS_AS(equalsMaskPayloadHeader(h3, decodeMaskPayloadHeader(encodeMaskPayloadHeader(h3))),
                     MaskCompressionException);

    std::array<unsigned char, 4> testArr1{0x0, 0x0, 0x0, 0x0};
    EXPECT_THROWS_AS(decodeMaskPayloadHeader(testArr1.data(), testArr1.size()), MaskCompressionException);
}

CASE("Test compute expected buffer sizes") {
    std::size_t bms1 = computeBufferSizeMaskBitMask(2);
    EXPECT(bms1 == (1 + 5));
    std::size_t bms2 = computeBufferSizeMaskBitMask(15);
    EXPECT(bms2 == (2 + 5));
    std::size_t bms3 = computeBufferSizeMaskBitMask(16);
    EXPECT(bms3 == (2 + 5));
    std::size_t bms4 = computeBufferSizeMaskBitMask(250);
    EXPECT(bms4 == (32 + 5));
    std::size_t bms5 = computeBufferSizeMaskBitMask(256);
    EXPECT(bms5 == (32 + 5));


    // Fill vector with 1 0 1 0 1 0 ....
    std::vector<float> v1(250);
    std::size_t v1Ind = 0;
    auto v1g = [&v1Ind]() { return ((++v1Ind) % 2) != 0 ? 1.0f : 0.0f; };
    std::generate(v1.begin(), v1.end(), v1g);

    MaskRunLengthProperties prop1 = computeMaskRunLengthProperties(v1.data(), v1.size());
    EXPECT(prop1.startValue == true);
    EXPECT(prop1.numValues == 250);
    EXPECT(prop1.numBitsPerInt == 1);
    EXPECT(prop1.bufSize == (32 + 5));  // 250 Bits can be stored in 32 bytes (256 Bits) + 5 bytes for the header


    // Fill vector with 1 0 0 0 0 0 0 1 0 0 0 ....
    // the last bits are 1 0
    std::vector<float> v2(250);
    std::size_t v2Ind = 7;
    auto v2g = [&v2Ind]() { return ((++v2Ind) % 8 == 0) ? 1.0f : 0.0f; };
    std::generate(v2.begin(), v2.end(), v2g);

    MaskRunLengthProperties prop2 = computeMaskRunLengthProperties(v2.data(), v2.size());
    EXPECT(prop2.startValue == true);
    EXPECT(prop2.numValues == 64);
    EXPECT(prop2.numBitsPerInt == 3);
    EXPECT(prop2.bufSize == (24 + 5));

    // Fill vector with  0 0 0 0 1 0 0 ... (20x 0) .. 0 1
    std::vector<float> v3(250);
    std::size_t v3Ind = 15;
    auto v3g = [&v3Ind]() { return ((++v3Ind) % 20 == 0) ? 1.0f : 0.0f; };
    std::generate(v3.begin(), v3.end(), v3g);

    MaskRunLengthProperties prop3 = computeMaskRunLengthProperties(v3.data(), v3.size());
    std::cout << "startValue " << prop3.startValue << std::endl;
    std::cout << "numValues " << prop3.numValues << std::endl;
    std::cout << "numBitsPerInt " << prop3.numBitsPerInt << std::endl;
    std::cout << "bufSize " << prop3.bufSize << std::endl;
    EXPECT(prop3.startValue == false);
    EXPECT(prop3.numValues == 27);
    EXPECT(prop3.numBitsPerInt == 5);
    EXPECT(prop3.bufSize == (17 + 5));


    // Fill large vector with increasing running numbers from 2**10 to 2**20 (the last one is cut ofc)
    std::size_t v4s = 1 << 20;
    std::vector<float> v4(v4s);
    std::size_t v4Ind = 0;
    std::size_t v4NextToggle = 1024;
    bool v4Val = true;
    auto v4g = [&v4Ind, &v4Val, &v4NextToggle]() {
        ++v4Ind;
        if (v4Ind > v4NextToggle) {
            v4Val = !v4Val;
            v4NextToggle *= 2;
            v4Ind = 1;
        }
        return v4Val;
    };
    std::generate(v4.begin(), v4.end(), v4g);

    MaskRunLengthProperties prop4 = computeMaskRunLengthProperties(v4.data(), v4.size());
    std::cout << "startValue " << prop4.startValue << std::endl;
    std::cout << "numValues " << prop4.numValues << std::endl;
    std::cout << "numBitsPerInt " << prop4.numBitsPerInt << std::endl;
    std::cout << "bufSize " << prop4.bufSize << std::endl;
    EXPECT(prop4.startValue == true);
    EXPECT(prop4.numValues == 11);  // From 2**10 to 2**19 -> 10 numbers + 1 last number that fills up to 1**20
    EXPECT(
        prop4.numBitsPerInt
        == 19);  // largest value to fit in is 1 << 19.. (20bits). But because 0 is not used, encoding derements by one
    EXPECT(prop4.bufSize == (27 + 5));
}

CASE("Test encode/decode bitmask") {
    // Fill vector with 1 0 1 0 1 0 ....
    std::vector<float> v1(250);
    std::size_t v1Ind = 0;
    auto v1g = [&v1Ind]() { return ((++v1Ind) % 2) != 0 ? 1.0f : 0.0f; };
    std::generate(v1.begin(), v1.end(), v1g);

    eckit::Buffer b1 = encodeMaskBitMask(v1.data(), v1.size());

    // for(unsigned int i=0; i < b1.size(); ++i) {
    //     std::cout << "b1[" << i << "]: " << std::hex << std::setiosflags (std::ios::showbase) << ((int) b1[i]) <<
    //     std::resetiosflags(std::ios::hex) << std::endl;
    // }

    EncodedMaskPayload em1(b1);
    std::size_t i1 = 0;
    for (bool v : em1) {
        // std::cout << i1 << ": " << v << std::endl;
        EXPECT(v == static_cast<bool>(v1[i1]));
        ++i1;
    }


    // Fill vector with 1 0 0 0 0 0 0 0 1 ....
    std::vector<float> v2(250);
    std::size_t v2Ind = 7;
    auto v2g = [&v2Ind]() { return ((++v2Ind) % 8 == 0) ? 1.0f : 0.0f; };
    std::generate(v2.begin(), v2.end(), v2g);

    eckit::Buffer b2 = encodeMaskBitMask(v2.data(), v2.size());

    // for(unsigned int i=0; i < b2.size(); ++i) {
    //     std::cout << "b2[" << i << "]: " << std::hex << std::setiosflags (std::ios::showbase) << ((int) b2[i]) <<
    //     std::resetiosflags(std::ios::hex) << std::endl;
    // }

    EncodedMaskPayload em2(b2);
    std::size_t i2 = 0;
    for (bool v : em2) {
        // std::cout << i2 << ": " << v << std::endl;
        EXPECT(v == static_cast<bool>(v2[i2]));
        ++i2;
    }


    // Fill vector with  0 0 0 0 1 0 0 ... (20x 0) .. 0 1
    std::vector<float> v3(250);
    std::size_t v3Ind = 15;
    auto v3g = [&v3Ind]() { return ((++v3Ind) % 20 == 0) ? 1.0f : 0.0f; };
    std::generate(v3.begin(), v3.end(), v3g);

    eckit::Buffer b3 = encodeMaskBitMask(v3.data(), v3.size());

    // for(unsigned int i=0; i < b3.size(); ++i) {
    //     std::cout << "b3[" << i << "]: " << std::hex << std::setiosflags (std::ios::showbase) << ((int) b3[i]) <<
    //     std::resetiosflags(std::ios::hex) << std::endl;
    // }

    EncodedMaskPayload em3(b3);
    std::size_t i3 = 0;
    for (bool v : em3) {
        // std::cout << i3 << ": " << v << std::endl;
        EXPECT(v == static_cast<bool>(v3[i3]));
        ++i3;
    }
}


CASE("Test encode/decode run length") {
    // Fill vector with 1 0 1 0 1 0 ....
    std::vector<float> v1(250);
    std::size_t v1Ind = 0;
    auto v1g = [&v1Ind]() { return ((++v1Ind) % 2) != 0 ? 1.0f : 0.0f; };
    std::generate(v1.begin(), v1.end(), v1g);

    eckit::Buffer b1 = encodeMaskRunLength(v1.data(), v1.size());

    // for (unsigned int i = 0; i < b1.size(); ++i) {
    //     std::cout << "b1[" << i << "]: " << std::hex << std::setiosflags(std::ios::showbase) << ((int)b1[i])
    //               << std::resetiosflags(std::ios::hex) << std::endl;
    // }

    EncodedMaskPayload em1(b1);
    std::size_t i1 = 0;
    for (bool v : em1) {
        // std::cout << i1 << ": " << v << " - " << static_cast<bool>(v1[i1]) << std::endl;
        EXPECT(v == static_cast<bool>(v1[i1]));
        ++i1;
    }


    // Fill vector with 1 0 0 0 0 0 0 0 1 ....
    std::vector<float> v2(250);
    std::size_t v2Ind = 7;
    auto v2g = [&v2Ind]() { return ((++v2Ind) % 8 == 0) ? 1.0f : 0.0f; };
    std::generate(v2.begin(), v2.end(), v2g);

    eckit::Buffer b2 = encodeMaskRunLength(v2.data(), v2.size());

    // for(unsigned int i=0; i < b2.size(); ++i) {
    //     std::cout << "b2[" << i << "]: " << std::hex << std::setiosflags (std::ios::showbase) << ((int) b2[i]) <<
    //     std::resetiosflags(std::ios::hex) << std::endl;
    // }

    EncodedMaskPayload em2(b2);
    std::size_t i2 = 0;
    for (bool v : em2) {
        // std::cout << i2 << ": " << v << std::endl;
        EXPECT(v == static_cast<bool>(v2[i2]));
        ++i2;
    }


    // Fill vector with  0 0 0 0 1 0 0 ... (20x 0) .. 0 1
    std::vector<float> v3(250);
    std::size_t v3Ind = 15;
    auto v3g = [&v3Ind]() { return ((++v3Ind) % 20 == 0) ? 1.0f : 0.0f; };
    std::generate(v3.begin(), v3.end(), v3g);

    eckit::Buffer b3 = encodeMaskRunLength(v3.data(), v3.size());

    // for(unsigned int i=0; i < b3.size(); ++i) {
    //     std::cout << "b3[" << i << "]: " << std::hex << std::setiosflags (std::ios::showbase) << ((int) b3[i]) <<
    //     std::resetiosflags(std::ios::hex) << std::endl;
    // }

    EncodedMaskPayload em3(b3);
    std::size_t i3 = 0;
    for (bool v : em3) {
        // std::cout << i3 << ": " << v << " - " << static_cast<bool>(v3[i3]) << std::endl;
        EXPECT(v == static_cast<bool>(v3[i3]));
        ++i3;
    }


    // Fill large vector with increasing running numbers from 2**10 to 2**20 (the last one is cut ofc)
    std::size_t v4s = 1 << 20;
    std::vector<float> v4(v4s);
    std::size_t v4Ind = 0;
    std::size_t v4NextToggle = 1024;
    bool v4Val = true;
    auto v4g = [&v4Ind, &v4Val, &v4NextToggle]() {
        ++v4Ind;
        if (v4Ind > v4NextToggle) {
            v4Val = !v4Val;
            v4NextToggle *= 2;
            v4Ind = 1;
        }
        return v4Val;
    };
    std::generate(v4.begin(), v4.end(), v4g);

    eckit::Buffer b4 = encodeMaskRunLength(v4.data(), v4.size());

    // for(unsigned int i=0; i < b4.size(); ++i) {
    //     std::cout << "b4[" << i << "]: " << std::hex << std::setiosflags (std::ios::showbase) << ((int) b4[i]) <<
    //     std::resetiosflags(std::ios::hex) << std::endl;
    // }

    EncodedMaskPayload em4(b4);
    std::size_t i4 = 0;
    for (bool v : em4) {
        // std::cout << i4 << ": " << v << std::endl;
        EXPECT(v == static_cast<bool>(v4[i4]));
        ++i4;
    }
}

}  // namespace test
}  // namespace multio

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}
