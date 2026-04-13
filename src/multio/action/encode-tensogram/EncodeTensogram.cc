/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "EncodeTensogram.h"

#include <algorithm>
#include <cmath>
#include <sstream>
#include <vector>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"
#include "eckit/utils/Overloaded.h"

#include "multio/LibMultio.h"
#include "multio/message/Message.h"
#include "multio/util/PrecisionTag.h"

// tensogram.hpp includes tensogram.h inside extern "C" { ... }
// We must NOT include tensogram.h separately — that would give it C++ linkage.
#include "tensogram.hpp"


namespace multio::action::encode_tensogram {

// Note: we do NOT use 'using message::Metadata' here because tensogram.hpp
// brings in tensogram::metadata which creates a name collision.
using message::Message;
using message::Peer;

namespace {

//----------------------------------------------------------------------------------------------------------------------
// Metadata key constants — well-known MARS keys to extract into base[0].mars
//----------------------------------------------------------------------------------------------------------------------

// clang-format off
const std::vector<std::string> marsKeys = {
    "class", "type", "stream", "expver",
    "date", "time", "step",
    "param", "paramId", "shortName",
    "levtype", "levelist", "level",
    "domain", "number",
};
// clang-format on

//----------------------------------------------------------------------------------------------------------------------
// JSON helpers — minimal escaping for string values
//----------------------------------------------------------------------------------------------------------------------

/// Escape a string for JSON (handles backslash, double-quote, control characters).
std::string jsonEscape(const std::string& s) {
    std::string out;
    out.reserve(s.size() + 8);
    for (char c : s) {
        switch (c) {
            case '"':
                out += "\\\"";
                break;
            case '\\':
                out += "\\\\";
                break;
            case '\n':
                out += "\\n";
                break;
            case '\r':
                out += "\\r";
                break;
            case '\t':
                out += "\\t";
                break;
            default:
                out += c;
                break;
        }
    }
    return out;
}

/// Emit a single metadata value into JSON.
/// Dispatches on the MetadataValue type: string → quoted, numeric → raw, bool → true/false.
void emitJsonValue(std::ostringstream& os, const message::MetadataValue& val) {
    val.visit(eckit::Overloaded{
        [&](std::nullptr_t) { os << "null"; },
        [&](bool v) { os << (v ? "true" : "false"); },
        [&](std::int64_t v) { os << v; },
        [&](double v) {
            if (std::isfinite(v)) {
                os << v;
            }
            else {
                os << "null";
            }
        },
        [&](const std::string& v) { os << '"' << jsonEscape(v) << '"'; },
        // For vectors of integers (e.g., levelist), emit as a JSON array
        [&](const std::vector<std::int64_t>& v) {
            os << '[';
            for (size_t i = 0; i < v.size(); ++i) {
                if (i > 0)
                    os << ',';
                os << v[i];
            }
            os << ']';
        },
        // For vectors of doubles
        [&](const std::vector<double>& v) {
            os << '[';
            for (size_t i = 0; i < v.size(); ++i) {
                if (i > 0)
                    os << ',';
                os << v[i];
            }
            os << ']';
        },
        // For nested metadata, emit as nested JSON object
        [&](const message::BaseMetadata& nested) {
            os << '{';
            bool first = true;
            for (const auto& [key, v] : nested) {
                if (!first)
                    os << ',';
                first = false;
                os << '"' << jsonEscape(key) << "\":";
                emitJsonValue(os, v);
            }
            os << '}';
        },
        // Catch-all for types we don't handle (e.g., vector<string>)
        [&](const auto&) { os << "null"; },
    });
}

/// Build the "base[0]" metadata object from multio Metadata.
/// MARS keys go under "mars", everything else goes at the top level.
std::string buildBaseEntry(const message::Metadata& md) {
    std::ostringstream marsJson;
    std::ostringstream extraJson;
    bool firstMars = true;
    bool firstExtra = true;

    // Collect known MARS keys → mars sub-object; everything else → top level
    for (const auto& [key, val] : md) {
        // The key type is PrehashedKey<std::string>; extract the string for comparisons
        const std::string& keyStr = static_cast<const std::string&>(key);

        // Skip internal/routing keys that are not meaningful in tensogram metadata
        if (keyStr == "misc-globalSize" || keyStr == "misc-precision" || keyStr == "bitmapPresent"
            || keyStr == "missingValue" || keyStr == "encoder-overwrites" || keyStr == "globalSize"
            || keyStr == "precision") {
            continue;
        }

        bool isMarsKey = std::find(marsKeys.begin(), marsKeys.end(), keyStr) != marsKeys.end();

        if (isMarsKey) {
            if (!firstMars)
                marsJson << ',';
            firstMars = false;
            marsJson << '"' << jsonEscape(keyStr) << "\":";
            emitJsonValue(marsJson, val);
        }
        else {
            if (!firstExtra)
                extraJson << ',';
            firstExtra = false;
            extraJson << '"' << jsonEscape(keyStr) << "\":";
            emitJsonValue(extraJson, val);
        }
    }

    std::ostringstream entry;
    entry << '{';
    bool needsComma = false;

    if (!firstMars) {
        entry << "\"mars\":{" << marsJson.str() << '}';
        needsComma = true;
    }

    if (!firstExtra) {
        if (needsComma)
            entry << ',';
        entry << extraJson.str();
    }

    entry << '}';
    return entry.str();
}

}  // namespace

//----------------------------------------------------------------------------------------------------------------------
// Constructor
//----------------------------------------------------------------------------------------------------------------------

EncodeTensogram::EncodeTensogram(const ComponentConfiguration& compConf) :
    ChainedAction{compConf},
    encoding_{compConf.parsedConfig().getString("encoding", "simple_packing")},
    filter_{compConf.parsedConfig().getString("filter", "none")},
    compression_{compConf.parsedConfig().getString("compression", "szip")},
    hashAlgo_{compConf.parsedConfig().getString("hash", "xxh3")},
    bitsPerValue_{static_cast<uint32_t>(compConf.parsedConfig().getInt("bits-per-value", 16))},
    decimalScaleFactor_{static_cast<int32_t>(compConf.parsedConfig().getInt("decimal-scale-factor", 0))} {

    // Validate configuration
    if (encoding_ != "none" && encoding_ != "simple_packing") {
        throw eckit::UserError(
            "EncodeTensogram: unsupported encoding '" + encoding_ + "'. Must be 'none' or 'simple_packing'.", Here());
    }
    if (filter_ != "none" && filter_ != "shuffle") {
        throw eckit::UserError("EncodeTensogram: unsupported filter '" + filter_ + "'. Must be 'none' or 'shuffle'.",
                               Here());
    }
    if (compression_ != "none" && compression_ != "szip" && compression_ != "zstd" && compression_ != "lz4") {
        throw eckit::UserError("EncodeTensogram: unsupported compression '" + compression_
                                   + "'. Must be 'none', 'szip', 'zstd', or 'lz4'.",
                               Here());
    }
    if (encoding_ == "simple_packing" && bitsPerValue_ == 0) {
        throw eckit::UserError("EncodeTensogram: bits-per-value must be > 0 for simple_packing encoding.", Here());
    }

    LOG_DEBUG_LIB(LibMultio) << "EncodeTensogram: encoding=" << encoding_ << " filter=" << filter_
                             << " compression=" << compression_ << " bits-per-value=" << bitsPerValue_
                             << " decimal-scale-factor=" << decimalScaleFactor_ << " hash=" << hashAlgo_ << std::endl;
}

//----------------------------------------------------------------------------------------------------------------------
// JSON builders for the tensogram C API
//----------------------------------------------------------------------------------------------------------------------

std::string EncodeTensogram::buildEncodeJson(const message::Metadata& md, size_t globalSize, const std::string& dtype,
                                             const std::string& byteOrder, double referenceValue,
                                             int32_t binaryScaleFactor) const {
    std::ostringstream json;
    json << '{';

    // Version (required)
    json << "\"version\":2,";

    // Descriptors array (one object)
    json << "\"descriptors\":[{";
    json << "\"type\":\"ndarray\",";
    json << "\"ndim\":1,";
    json << "\"shape\":[" << globalSize << "],";
    // Strides: for float64 input to simple_packing, stride = 8 bytes (the input dtype)
    json << "\"strides\":[8],";
    json << "\"dtype\":\"" << dtype << "\",";
    json << "\"byte_order\":\"" << byteOrder << "\",";
    json << "\"encoding\":\"" << encoding_ << "\",";
    json << "\"filter\":\"" << filter_ << "\",";
    json << "\"compression\":\"" << compression_ << "\"";

    // Szip compression parameters (required by the szip codec)
    if (compression_ == "szip") {
        // Reference Sample Interval: must divide the total number of packed values.
        // 32 is the most common default (matches GRIB szip usage).
        json << ",\"szip_rsi\":32";
        json << ",\"szip_block_size\":8";
        json << ",\"szip_flags\":4";  // EC (entropy coding)
    }

    // Simple packing parameters
    if (encoding_ == "simple_packing") {
        json << ",\"bits_per_value\":" << bitsPerValue_;
        json << ",\"decimal_scale_factor\":" << decimalScaleFactor_;
        json << ",\"reference_value\":" << referenceValue;
        json << ",\"binary_scale_factor\":" << binaryScaleFactor;
    }

    json << "}],";  // end descriptors

    // Base array (per-object metadata — MARS keys + extras)
    json << "\"base\":[" << buildBaseEntry(md) << "]";

    json << '}';
    return json.str();
}

std::string EncodeTensogram::buildEncodeJsonRaw(const message::Metadata& md, size_t globalSize,
                                                const std::string& dtype, const std::string& byteOrder,
                                                size_t bytesPerElement) const {
    std::ostringstream json;
    json << '{';

    json << "\"version\":2,";

    json << "\"descriptors\":[{";
    json << "\"type\":\"ndarray\",";
    json << "\"ndim\":1,";
    json << "\"shape\":[" << globalSize << "],";
    json << "\"strides\":[" << bytesPerElement << "],";
    json << "\"dtype\":\"" << dtype << "\",";
    json << "\"byte_order\":\"" << byteOrder << "\",";
    json << "\"encoding\":\"none\",";
    json << "\"filter\":\"" << filter_ << "\",";
    json << "\"compression\":\"" << compression_ << "\"";

    // Szip compression parameters (required by the szip codec)
    if (compression_ == "szip") {
        json << ",\"szip_rsi\":32";
        json << ",\"szip_block_size\":8";
        json << ",\"szip_flags\":4";
    }

    json << "}],";

    json << "\"base\":[" << buildBaseEntry(md) << "]";

    json << '}';
    return json.str();
}

//----------------------------------------------------------------------------------------------------------------------
// Core encoding logic
//----------------------------------------------------------------------------------------------------------------------

void EncodeTensogram::executeImpl(Message msg) {

    // Non-Field messages pass through unchanged
    if (msg.tag() != Message::Tag::Field) {
        executeNext(std::move(msg));
        return;
    }

    if (msg.payload().size() == 0) {
        throw eckit::SeriousBug("EncodeTensogram: Message has empty payload - no values to encode", Here());
    }

    const auto& md = msg.metadata();
    const auto globalSize = static_cast<size_t>(msg.globalSize());

    // Determine byte order for this platform
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    const std::string byteOrder = "little";
#else
    const std::string byteOrder = "big";
#endif

    // Dispatch on precision (float32 / float64)
    auto encoded = dispatchPrecisionTag(msg.precision(), [&](auto pt) -> std::vector<uint8_t> {
        using Precision = typename decltype(pt)::type;

        const auto* values = reinterpret_cast<const Precision*>(msg.payload().data());
        const size_t numValues = msg.payload().size() / sizeof(Precision);

        if (encoding_ == "simple_packing") {
            // simple_packing requires float64 input — convert if needed
            std::vector<double> doubleValues;
            const double* doublePtr = nullptr;

            if constexpr (std::is_same_v<Precision, double>) {
                doublePtr = values;
            }
            else {
                // Convert float32 → float64
                doubleValues.resize(numValues);
                std::copy(values, values + numValues, doubleValues.begin());
                doublePtr = doubleValues.data();
            }

            // Compute packing parameters
            double referenceValue = 0.0;
            int32_t binaryScaleFactor = 0;

            tgm_error err = tgm_simple_packing_compute_params(doublePtr, numValues, bitsPerValue_, decimalScaleFactor_,
                                                              &referenceValue, &binaryScaleFactor);
            if (err != TGM_ERROR_OK) {
                std::ostringstream oss;
                oss << "EncodeTensogram: tgm_simple_packing_compute_params failed: " << tgm_error_string(err);
                const char* detail = tgm_last_error();
                if (detail && detail[0] != '\0') {
                    oss << " (" << detail << ")";
                }
                throw eckit::SeriousBug(oss.str(), Here());
            }

            // Build JSON and encode
            std::string json = buildEncodeJson(md, globalSize, "float64", byteOrder, referenceValue, binaryScaleFactor);

            const auto* dataPtr = reinterpret_cast<const uint8_t*>(doublePtr);
            size_t dataLen = numValues * sizeof(double);

            return tensogram::encode(json, {{dataPtr, dataLen}}, tensogram::encode_options{hashAlgo_});
        }
        else {
            // encoding = "none" — pass raw data in native precision
            std::string dtype = std::is_same_v<Precision, double> ? "float64" : "float32";
            size_t bytesPerElement = sizeof(Precision);

            std::string json = buildEncodeJsonRaw(md, globalSize, dtype, byteOrder, bytesPerElement);

            const auto* dataPtr = reinterpret_cast<const uint8_t*>(values);
            size_t dataLen = numValues * sizeof(Precision);

            return tensogram::encode(json, {{dataPtr, dataLen}}, tensogram::encode_options{hashAlgo_});
        }
    });

    // Move encoded bytes into an eckit::Buffer
    eckit::Buffer buf(encoded.size());
    std::memcpy(buf.data(), encoded.data(), encoded.size());

    // Retain MARS metadata on the output message for downstream routing
    // (following the MTG2 encoder pattern)
    message::Metadata outputMd{md};

    executeNext(
        Message{Message::Header{Message::Tag::Field, Peer{msg.source()}, Peer{msg.destination()}, std::move(outputMd)},
                std::move(buf)});
}

//----------------------------------------------------------------------------------------------------------------------

void EncodeTensogram::print(std::ostream& os) const {
    os << "EncodeTensogram{encoding=" << encoding_ << ", compression=" << compression_
       << ", bits-per-value=" << bitsPerValue_ << "}";
}

//----------------------------------------------------------------------------------------------------------------------
// Factory self-registration — MUST be in a SHARED library for this to work at dlopen time
//----------------------------------------------------------------------------------------------------------------------

static ActionBuilder<EncodeTensogram> EncodeTensogramBuilder("encode-tensogram");

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio::action::encode_tensogram
