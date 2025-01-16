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

#include <algorithm>
#include <iomanip>
#include <random>

namespace multio::test {

using multio::domain::computeBufferSizeMaskBitMask;
using multio::domain::computeMaskRunLengthProperties;
using multio::domain::decodeMaskPayloadHeader;
using multio::domain::EncodedBitMaskPayload;
using multio::domain::EncodedRunLengthPayload;
using multio::domain::encodeMask;
using multio::domain::encodeMaskBitMask;
using multio::domain::encodeMaskPayloadHeader;
using multio::domain::encodeMaskRunLength;
using multio::domain::MaskCompressionException;
using multio::domain::MaskPayloadFormat;
using multio::domain::MaskPayloadHeader;
using multio::domain::MaskPayloadIterator;
using multio::domain::MaskRunLengthProperties;
using multio::domain::RunLengthIterator;

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
    eckit::Log::info() << "startValue " << prop3.startValue << std::endl
                       << "numValues " << prop3.numValues << std::endl
                       << "numBitsPerInt " << prop3.numBitsPerInt << std::endl
                       << "bufSize " << prop3.bufSize << std::endl;
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
    eckit::Log::info() << "startValue " << prop4.startValue << std::endl
                       << "numValues " << prop4.numValues << std::endl
                       << "numBitsPerInt " << prop4.numBitsPerInt << std::endl
                       << "bufSize " << prop4.bufSize << std::endl;
    EXPECT(prop4.startValue == true);
    // From 2**10 to 2**19 -> 10 numbers + 1 last number that fills up to 1**20
    EXPECT(prop4.numValues == 11);
    // The largest value to fit in is 1 << 19.. (20bits). But because 0 is not used, encoding derements by one
    EXPECT(prop4.numBitsPerInt == 19);
    EXPECT(prop4.bufSize == (27 + 5));


    // Fill vector with 1
    std::size_t v5s = 1 << 25;
    std::vector<float> v5(v5s);
    auto v5g = []() { return true; };
    std::generate(v5.begin(), v5.end(), v5g);

    MaskRunLengthProperties prop5 = computeMaskRunLengthProperties(v5.data(), v5.size());
    eckit::Log::info() << "startValue " << prop5.startValue << std::endl
                       << "numValues " << prop5.numValues << std::endl
                       << "numBitsPerInt " << prop5.numBitsPerInt << std::endl
                       << "bufSize " << prop5.bufSize << std::endl;
    EXPECT(prop5.startValue == true);
    EXPECT(prop5.numValues == 1);
    EXPECT(prop5.numBitsPerInt == 25);
    EXPECT(prop5.bufSize == (4 + 5));


    // Fill vector with 0
    std::size_t v6s = 1 << 25;
    std::vector<float> v6(v6s);
    auto v6g = []() { return false; };
    std::generate(v6.begin(), v6.end(), v6g);

    MaskRunLengthProperties prop6 = computeMaskRunLengthProperties(v6.data(), v6.size());
    eckit::Log::info() << "startValue " << prop6.startValue << std::endl
                       << "numValues " << prop6.numValues << std::endl
                       << "numBitsPerInt " << prop6.numBitsPerInt << std::endl
                       << "bufSize " << prop6.bufSize << std::endl;
    EXPECT(prop6.startValue == false);
    EXPECT(prop6.numValues == 1);
    EXPECT(prop6.numBitsPerInt == 25);
    EXPECT(prop6.bufSize == (4 + 5));
}

CASE("Test encode/decode bitmask") {
    // Fill vector with 1 0 1 0 1 0 ....
    std::vector<float> v1(250);
    std::size_t v1Ind = 0;
    auto v1g = [&v1Ind]() { return ((++v1Ind) % 2) != 0 ? 1.0f : 0.0f; };
    std::generate(v1.begin(), v1.end(), v1g);

    eckit::Buffer b1 = encodeMaskBitMask(v1.data(), v1.size());

    EncodedBitMaskPayload em1(b1);
    std::size_t i1 = 0;
    for (bool v : em1) {
        EXPECT(v == static_cast<bool>(v1[i1]));
        ++i1;
    }
    EXPECT(i1 == 250);

    // Fill vector with 1 0 0 0 0 0 0 0 1 ....
    std::vector<float> v2(250);
    std::size_t v2Ind = 7;
    auto v2g = [&v2Ind]() { return ((++v2Ind) % 8 == 0) ? 1.0f : 0.0f; };
    std::generate(v2.begin(), v2.end(), v2g);

    eckit::Buffer b2 = encodeMaskBitMask(v2.data(), v2.size());

    EncodedBitMaskPayload em2(b2);
    std::size_t i2 = 0;
    for (bool v : em2) {
        EXPECT(v == static_cast<bool>(v2[i2]));
        ++i2;
    }
    EXPECT(i2 == 250);


    // Fill vector with  0 0 0 0 1 0 0 ... (20x 0) .. 0 1
    std::vector<float> v3(250);
    std::size_t v3Ind = 15;
    auto v3g = [&v3Ind]() { return ((++v3Ind) % 20 == 0) ? 1.0f : 0.0f; };
    std::generate(v3.begin(), v3.end(), v3g);

    eckit::Buffer b3 = encodeMaskBitMask(v3.data(), v3.size());

    EncodedBitMaskPayload em3(b3);
    std::size_t i3 = 0;
    for (bool v : em3) {
        EXPECT(v == static_cast<bool>(v3[i3]));
        ++i3;
    }
    EXPECT(i3 == 250);
}


CASE("Test encode/decode run length") {
    // Fill vector with 1 0 1 0 1 0 ....
    std::vector<float> v1(250);
    std::size_t v1Ind = 0;
    auto v1g = [&v1Ind]() { return ((++v1Ind) % 2) != 0 ? 1.0f : 0.0f; };
    std::generate(v1.begin(), v1.end(), v1g);

    eckit::Buffer b1 = encodeMaskRunLength(v1.data(), v1.size());

    EncodedBitMaskPayload em1(b1);
    std::size_t i1 = 0;
    for (bool v : em1) {
        EXPECT(v == static_cast<bool>(v1[i1]));
        ++i1;
    }
    EXPECT(i1 == 250);

    EncodedRunLengthPayload rl1(b1);
    i1 = 0;
    for (const auto& pair : rl1) {
        for (std::size_t pi = 0; pi < pair.second; ++pi) {
            EXPECT(pair.first == static_cast<bool>(v1[i1]));
            ++i1;
        }
    }
    EXPECT(i1 == 250);


    // Fill vector with 1 0 0 0 0 0 0 0 1 ....
    std::vector<float> v2(250);
    std::size_t v2Ind = 7;
    auto v2g = [&v2Ind]() { return ((++v2Ind) % 8 == 0) ? 1.0f : 0.0f; };
    std::generate(v2.begin(), v2.end(), v2g);

    eckit::Buffer b2 = encodeMaskRunLength(v2.data(), v2.size());

    EncodedBitMaskPayload em2(b2);
    std::size_t i2 = 0;
    for (bool v : em2) {
        EXPECT(v == static_cast<bool>(v2[i2]));
        ++i2;
    }
    EXPECT(i2 == 250);

    EncodedRunLengthPayload rl2(b2);
    i2 = 0;
    for (const auto& pair : rl2) {
        for (std::size_t pi = 0; pi < pair.second; ++pi) {
            EXPECT(pair.first == static_cast<bool>(v2[i2]));
            ++i2;
        }
    }
    EXPECT(i2 == 250);


    // Fill vector with  0 0 0 0 1 0 0 ... (20x 0) .. 0 1
    std::vector<float> v3(250);
    std::size_t v3Ind = 15;
    auto v3g = [&v3Ind]() { return ((++v3Ind) % 20 == 0) ? 1.0f : 0.0f; };
    std::generate(v3.begin(), v3.end(), v3g);

    eckit::Buffer b3 = encodeMaskRunLength(v3.data(), v3.size());

    EncodedBitMaskPayload em3(b3);
    std::size_t i3 = 0;
    for (bool v : em3) {
        EXPECT(v == static_cast<bool>(v3[i3]));
        ++i3;
    }
    EXPECT(i3 == 250);

    EncodedRunLengthPayload rl3(b3);
    i3 = 0;
    for (const auto& pair : rl3) {
        for (std::size_t pi = 0; pi < pair.second; ++pi) {
            EXPECT(pair.first == static_cast<bool>(v3[i3]));
            ++i3;
        }
    }
    EXPECT(i3 == 250);


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

    EncodedBitMaskPayload em4(b4);
    std::size_t i4 = 0;
    for (bool v : em4) {
        EXPECT(v == static_cast<bool>(v4[i4]));
        ++i4;
    }
    EXPECT(i4 == v4s);

    EncodedRunLengthPayload rl4(b4);
    i4 = 0;
    for (const auto& pair : rl4) {
        for (std::size_t pi = 0; pi < pair.second; ++pi) {
            EXPECT(pair.first == static_cast<bool>(v4[i4]));
            ++i4;
        }
    }
    EXPECT(i4 == v4s);

    // Fill vector with 1
    std::size_t v5s = 1 << 25;
    std::vector<float> v5(v5s);
    auto v5g = []() { return true; };
    std::generate(v5.begin(), v5.end(), v5g);

    eckit::Buffer b5 = encodeMaskRunLength(v5.data(), v5.size());

    EncodedBitMaskPayload em5(b5);
    std::size_t i5 = 0;
    for (bool v : em5) {
        EXPECT(v == static_cast<bool>(v5[i5]));
        ++i5;
    }
    EXPECT(i5 == v5s);

    EncodedRunLengthPayload rl5(b5);
    i5 = 0;
    for (const auto& pair : rl5) {
        for (std::size_t pi = 0; pi < pair.second; ++pi) {
            EXPECT(pair.first == static_cast<bool>(v5[i5]));
            ++i5;
        }
    }
    EXPECT(i5 == v5s);


    // Fill vector with 0
    std::size_t v6s = 1 << 25;
    std::vector<float> v6(v6s);
    auto v6g = []() { return false; };
    std::generate(v6.begin(), v6.end(), v6g);

    eckit::Buffer b6 = encodeMaskRunLength(v6.data(), v6.size());

    EncodedBitMaskPayload em6(b6);
    std::size_t i6 = 0;
    for (bool v : em6) {
        EXPECT(v == static_cast<bool>(v6[i6]));
        ++i6;
    }
    EXPECT(i6 == v6s);

    EncodedRunLengthPayload rl6(b6);
    i6 = 0;
    for (const auto& pair : rl6) {
        for (std::size_t pi = 0; pi < pair.second; ++pi) {
            EXPECT(pair.first == static_cast<bool>(v6[i6]));
            ++i6;
        }
    }
    EXPECT(i6 == v6s);


    // Fill vector with sequences of 16 such that two consecutive numbers fill one byte and all information matches
    // exactly in the buffer without some bits left off
    std::size_t v7s = 1 << 6;
    std::vector<float> v7(v7s);
    unsigned int v7Ind = 0;
    bool v7Val = true;
    auto v7g = [&v7Ind, &v7Val]() {
        if (v7Ind == 0) {
            v7Ind = 16;
            v7Val = !v7Val;
        }
        --v7Ind;
        return v7Val;
    };
    std::generate(v7.begin(), v7.end(), v7g);


    eckit::Buffer b7 = encodeMaskRunLength(v7.data(), v7.size());

    EncodedBitMaskPayload em7(b7);
    std::size_t i7 = 0;
    for (bool v : em7) {
        EXPECT(v == static_cast<bool>(v7[i7]));
        ++i7;
    }
    EXPECT(i7 == v7s);

    EncodedRunLengthPayload rl7(b7);
    i7 = 0;
    for (const auto& pair : rl7) {
        for (std::size_t pi = 0; pi < pair.second; ++pi) {
            EXPECT(pair.first == static_cast<bool>(v7[i7]));
            ++i7;
        }
    }
    EXPECT(i7 == v7s);
}

CASE("Test encode/decode run length with a lot of combinations (random)") {
    constexpr unsigned int runs = 1 << 6;
    constexpr unsigned int randMaxCons = 1 << 20;

    std::random_device rd;   // a seed source for the random number engine
    std::mt19937 gen(rd());  // mersenne_twister_engine seeded with rd()
    std::uniform_int_distribution<> startDistrib(0, 1);


    for (unsigned int r = 1; r < runs; ++r) {
        // Fill vector with i ones and j zeros.
        std::vector<float> v(1 << 22);
        std::uniform_int_distribution<> distrib(1, randMaxCons / runs * (r + 1));

        bool vVal = startDistrib(gen);
        std::size_t vInd = distrib(gen);

        eckit::Log::info() << "Test encode/decode run length combination run: " << r << " and startVal " << vVal
                           << std::endl;

        auto vg = [&vInd, &vVal, &distrib, &gen]() {
            if (vInd == 0) {
                vInd = distrib(gen);
                vVal = !vVal;
            }
            --vInd;
            return vVal;
        };
        std::generate(v.begin(), v.end(), vg);

        eckit::Buffer b = encodeMaskRunLength(v.data(), v.size());

        EncodedBitMaskPayload em(b);
        std::size_t ib = 0;
        for (bool vEncDec : em) {
            EXPECT(vEncDec == static_cast<bool>(v[ib]));
            ++ib;
        }
    };
}

}  // namespace multio::test

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}
