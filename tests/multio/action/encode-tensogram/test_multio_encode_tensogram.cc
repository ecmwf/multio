/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Tiago Quintino
/// @date Jul 2025

/// Unit tests for the encode-tensogram action.
/// These tests verify that:
///   1. Field messages are encoded into valid Tensogram binary format
///   2. Metadata (MARS keys) is preserved in both the output message and the Tensogram payload
///   3. Non-Field messages pass through unchanged
///   4. The output can be decoded back and verified

#include <cmath>
#include <cstring>
#include <queue>
#include <vector>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/config/YAMLConfiguration.h"
#include "eckit/io/Buffer.h"
#include "eckit/testing/Test.h"

#include "multio/action/Plan.h"
#include "multio/config/MultioConfiguration.h"
#include "multio/message/Message.h"
#include "multio/message/Metadata.h"

// tensogram.hpp wraps tensogram.h in extern "C"
#include "tensogram.hpp"


namespace multio::test {

using multio::message::Message;
using multio::message::Peer;

//----------------------------------------------------------------------------------------------------------------------
// Helper: Create a synthetic float64 field with a smooth gradient
//----------------------------------------------------------------------------------------------------------------------

std::vector<double> makeSyntheticField(size_t size, double baseValue = 273.15) {
    std::vector<double> values(size);
    for (size_t i = 0; i < size; ++i) {
        values[i] = baseValue + 10.0 * std::sin(static_cast<double>(i) * 0.01);
    }
    return values;
}

//----------------------------------------------------------------------------------------------------------------------
// Helper: Create a multio Message with metadata and a double-precision payload
//----------------------------------------------------------------------------------------------------------------------

Message makeFieldMessage(const std::vector<double>& data) {
    message::Metadata md;
    md.set("name", std::string("2t"));
    md.set("param", std::string("167"));
    md.set("paramId", static_cast<std::int64_t>(167));
    md.set("class", std::string("od"));
    md.set("type", std::string("fc"));
    md.set("stream", std::string("oper"));
    md.set("expver", std::string("0001"));
    md.set("date", static_cast<std::int64_t>(20260701));
    md.set("time", static_cast<std::int64_t>(0));
    md.set("step", static_cast<std::int64_t>(6));
    md.set("levtype", std::string("sfc"));
    md.set("domain", std::string("g"));
    md.set("misc-globalSize", static_cast<std::int64_t>(data.size()));
    md.set("misc-precision", std::string("double"));

    eckit::Buffer payload(data.size() * sizeof(double));
    std::memcpy(payload.data(), data.data(), payload.size());

    return Message{Message::Header{Message::Tag::Field, Peer{"client", 0}, Peer{"server", 0}, std::move(md)},
                   std::move(payload)};
}

//----------------------------------------------------------------------------------------------------------------------
// Helper: Create config and run message through a plan, capture output via debug-sink
//----------------------------------------------------------------------------------------------------------------------

Message runThroughPlan(const std::string& encoding, const std::string& compression, int bitsPerValue, Message msg) {

    // Build plan YAML — on-error: recover lets us see the actual exception
    std::string yaml
        = "plans:\n"
          "  - name: test-encode-tensogram\n"
          "    on-error: recover\n"
          "    actions:\n"
          "      - type: encode-tensogram\n"
          "        on-error: recover\n"
          "        encoding: "
        + encoding
        + "\n"
          "        compression: "
        + compression
        + "\n"
          "        bits-per-value: "
        + std::to_string(bitsPerValue)
        + "\n"
          "        filter: none\n"
          "        hash: xxh3\n"
          "        next:\n"
          "          type: debug-sink\n";

    config::MultioConfiguration conf(eckit::LocalConfiguration{eckit::YAMLConfiguration{yaml}});
    auto& debugQueue = conf.debugSink();

    auto planConfigs = conf.parsedConfig().getSubConfigurations("plans");
    auto plans = action::Plan::makePlans(planConfigs, conf);
    ASSERT(plans.size() == 1);

    plans[0]->process(std::move(msg));

    ASSERT(!debugQueue.empty());
    auto result = std::move(debugQueue.front());
    debugQueue.pop();
    return result;
}

//----------------------------------------------------------------------------------------------------------------------
// TEST: Encoding with encoding=none produces a valid Tensogram message
//----------------------------------------------------------------------------------------------------------------------

CASE("EncodeTensogram: raw encoding produces valid tensogram message") {
    const size_t fieldSize = 1000;
    auto data = makeSyntheticField(fieldSize);
    auto msg = makeFieldMessage(data);

    auto encoded = runThroughPlan("none", "none", 16, std::move(msg));

    // The output payload should be a valid tensogram message
    EXPECT(encoded.tag() == Message::Tag::Field);
    EXPECT(encoded.payload().size() > 0);

    // Decode with tensogram C++ API and verify
    auto tgmMsg
        = tensogram::decode(reinterpret_cast<const uint8_t*>(encoded.payload().data()), encoded.payload().size());

    EXPECT(tgmMsg.num_objects() == 1);

    auto obj = tgmMsg.object(0);
    EXPECT(obj.ndim() == 1);
    EXPECT(obj.shape()[0] == fieldSize);
    EXPECT(std::string(obj.dtype_string()) == "float64");
    EXPECT(std::string(obj.encoding()) == "none");

    // Verify data round-trip: raw encoding should be lossless
    auto decoded = obj.data_as<double>();
    for (size_t i = 0; i < fieldSize; ++i) {
        EXPECT(decoded[i] == data[i]);
    }
}

//----------------------------------------------------------------------------------------------------------------------
// TEST: Encoding with simple_packing produces valid, compressed output
//----------------------------------------------------------------------------------------------------------------------

CASE("EncodeTensogram: simple_packing encoding produces valid tensogram message") {
    const size_t fieldSize = 1000;
    auto data = makeSyntheticField(fieldSize);
    auto msg = makeFieldMessage(data);

    auto encoded = runThroughPlan("simple_packing", "none", 16, std::move(msg));

    EXPECT(encoded.tag() == Message::Tag::Field);
    EXPECT(encoded.payload().size() > 0);

    // The encoded size should be smaller than raw (simple_packing compresses)
    EXPECT(encoded.payload().size() < fieldSize * sizeof(double));

    // Decode and verify data is within packing tolerance
    auto tgmMsg
        = tensogram::decode(reinterpret_cast<const uint8_t*>(encoded.payload().data()), encoded.payload().size());

    EXPECT(tgmMsg.num_objects() == 1);

    auto obj = tgmMsg.object(0);
    EXPECT(std::string(obj.encoding()) == "simple_packing");

    // Decoded values should be float64 and within packing tolerance
    auto decoded = obj.data_as<double>();
    double maxError = 0.0;
    for (size_t i = 0; i < fieldSize; ++i) {
        maxError = std::max(maxError, std::abs(decoded[i] - data[i]));
    }
    // At 16 bits per value with this synthetic field's range of ~20 (273.15 +/- 10), 1.0 is a loose tolerance
    EXPECT(maxError < 1.0);
}

//----------------------------------------------------------------------------------------------------------------------
// TEST: simple_packing + szip compression
//----------------------------------------------------------------------------------------------------------------------

CASE("EncodeTensogram: simple_packing with szip compression") {
    const size_t fieldSize = 2000;
    auto data = makeSyntheticField(fieldSize);
    auto msg = makeFieldMessage(data);

    auto encoded = runThroughPlan("simple_packing", "szip", 16, std::move(msg));

    EXPECT(encoded.tag() == Message::Tag::Field);
    EXPECT(encoded.payload().size() > 0);

    // Decode and verify
    auto tgmMsg
        = tensogram::decode(reinterpret_cast<const uint8_t*>(encoded.payload().data()), encoded.payload().size());

    EXPECT(tgmMsg.num_objects() == 1);
    auto obj = tgmMsg.object(0);
    EXPECT(std::string(obj.compression()) == "szip");

    // Verify data integrity (within packing tolerance)
    auto decoded = obj.data_as<double>();
    for (size_t i = 0; i < fieldSize; ++i) {
        EXPECT(std::abs(decoded[i] - data[i]) < 1.0);
    }
}

//----------------------------------------------------------------------------------------------------------------------
// TEST: MARS metadata is preserved in the tensogram message
//----------------------------------------------------------------------------------------------------------------------

CASE("EncodeTensogram: MARS metadata is preserved in tensogram payload") {
    const size_t fieldSize = 100;
    auto data = makeSyntheticField(fieldSize);
    auto msg = makeFieldMessage(data);

    auto encoded = runThroughPlan("none", "none", 16, std::move(msg));

    // Decode metadata from the tensogram payload
    auto meta = tensogram::decode_metadata(reinterpret_cast<const uint8_t*>(encoded.payload().data()),
                                           encoded.payload().size());

    EXPECT(meta.version() == 2);
    EXPECT(meta.num_objects() == 1);

    // Check MARS keys are present (dot-notation lookup in base[0])
    EXPECT(meta.get_string("mars.class") == std::string("od"));
    EXPECT(meta.get_string("mars.type") == std::string("fc"));
    EXPECT(meta.get_string("mars.stream") == std::string("oper"));
    EXPECT(meta.get_string("mars.param") == std::string("167"));
    EXPECT(meta.get_string("mars.levtype") == std::string("sfc"));
    EXPECT(meta.get_int("mars.step", -1) == 6);
    EXPECT(meta.get_int("mars.date", -1) == 20260701);
}

//----------------------------------------------------------------------------------------------------------------------
// TEST: MARS metadata is retained on the output Message (for downstream routing)
//----------------------------------------------------------------------------------------------------------------------

CASE("EncodeTensogram: output message retains MARS metadata for routing") {
    const size_t fieldSize = 100;
    auto data = makeSyntheticField(fieldSize);
    auto msg = makeFieldMessage(data);

    auto encoded = runThroughPlan("none", "none", 16, std::move(msg));

    // The output Message should still have MARS metadata for downstream routing
    const auto& md = encoded.metadata();
    EXPECT(md.get<std::string>("class") == "od");
    EXPECT(md.get<std::string>("type") == "fc");
    EXPECT(md.get<std::string>("name") == "2t");
}

//----------------------------------------------------------------------------------------------------------------------
// TEST: Non-Field messages pass through unchanged
//----------------------------------------------------------------------------------------------------------------------

CASE("EncodeTensogram: Flush messages pass through unchanged") {

    std::string yaml
        = "plans:\n"
          "  - name: test-passthrough\n"
          "    actions:\n"
          "      - type: encode-tensogram\n"
          "        encoding: none\n"
          "        compression: none\n"
          "        next:\n"
          "          type: debug-sink\n";

    config::MultioConfiguration conf(eckit::LocalConfiguration{eckit::YAMLConfiguration{yaml}});
    auto& debugQueue = conf.debugSink();

    auto planConfigs = conf.parsedConfig().getSubConfigurations("plans");
    auto plans = action::Plan::makePlans(planConfigs, conf);

    message::Metadata md;
    md.set("trigger", std::string("step"));
    md.set("step", static_cast<std::int64_t>(6));

    Message flushMsg{Message::Header{Message::Tag::Flush, Peer{"client", 0}, Peer{"server", 0}, std::move(md)}};

    plans[0]->process(std::move(flushMsg));

    ASSERT(!debugQueue.empty());
    auto result = std::move(debugQueue.front());
    debugQueue.pop();

    EXPECT(result.tag() == Message::Tag::Flush);
}

}  // namespace multio::test

//----------------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}
