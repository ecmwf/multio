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

#include <cmath>
#include <limits>
#include <sstream>
#include <unordered_set>
#include <vector>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/log/JSON.h"
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
// Well-known MARS keys to extract into base[0].mars
//----------------------------------------------------------------------------------------------------------------------

const std::unordered_set<std::string> marsKeys = {
    "class",   "type",      "stream",  "expver",   "date",  "time",   "step",   "param",
    "paramId", "shortName", "levtype", "levelist", "level", "domain", "number",
};

// Internal/routing keys that should be excluded from tensogram metadata
const std::unordered_set<std::string> skipKeys = {
    "misc-globalSize",    "misc-precision", "bitmapPresent", "missingValue",
    "encoder-overwrites", "globalSize",     "precision",
};

//----------------------------------------------------------------------------------------------------------------------
// Emit a MetadataValue into eckit::JSON.
// eckit::JSON handles string escaping (including all control characters).
//----------------------------------------------------------------------------------------------------------------------

void emitJsonValue(eckit::JSON& json, const message::MetadataValue& val) {
    val.visit(eckit::Overloaded{
        [&](std::nullptr_t) { json.null(); },
        [&](bool v) { json << v; },
        [&](std::int64_t v) { json << v; },
        [&](double v) {
            if (std::isfinite(v)) {
                json << v;
            }
            else {
                json.null();
            }
        },
        [&](const std::string& v) { json << v; },
        [&](const std::vector<std::int64_t>& v) {
            json.startList();
            for (auto x : v) {
                json << x;
            }
            json.endList();
        },
        [&](const std::vector<double>& v) {
            json.startList();
            for (auto x : v) {
                if (std::isfinite(x)) {
                    json << x;
                }
                else {
                    json.null();
                }
            }
            json.endList();
        },
        [&](const message::BaseMetadata& nested) {
            json.startObject();
            for (const auto& [key, v] : nested) {
                const std::string& keyStr = static_cast<const std::string&>(key);
                json << keyStr;
                emitJsonValue(json, v);
            }
            json.endObject();
        },
        // Catch-all for types we don't handle
        [&](const auto&) { json.null(); },
    });
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
    useSimplePacking_{false},
    bitsPerValue_{0},
    decimalScaleFactor_{0} {

    // Validate encoding
    if (encoding_ != "none" && encoding_ != "simple_packing") {
        throw eckit::UserError(
            "EncodeTensogram: unsupported encoding '" + encoding_ + "'. Must be 'none' or 'simple_packing'.", Here());
    }
    useSimplePacking_ = (encoding_ == "simple_packing");

    // Validate filter
    if (filter_ != "none" && filter_ != "shuffle") {
        throw eckit::UserError("EncodeTensogram: unsupported filter '" + filter_ + "'. Must be 'none' or 'shuffle'.",
                               Here());
    }

    // Validate compression
    if (compression_ != "none" && compression_ != "szip" && compression_ != "zstd" && compression_ != "lz4") {
        throw eckit::UserError("EncodeTensogram: unsupported compression '" + compression_
                                   + "'. Must be 'none', 'szip', 'zstd', or 'lz4'.",
                               Here());
    }

    // Validate hash algorithm
    if (hashAlgo_ != "xxh3" && !hashAlgo_.empty()) {
        throw eckit::UserError(
            "EncodeTensogram: unsupported hash '" + hashAlgo_ + "'. Must be 'xxh3' or '' (empty to disable).", Here());
    }

    // Validate bits-per-value: must be in [1, 64] for simple_packing
    auto bpvSigned = compConf.parsedConfig().getInt("bits-per-value", 16);
    if (bpvSigned < 1 || bpvSigned > 64) {
        throw eckit::UserError(
            "EncodeTensogram: bits-per-value must be between 1 and 64, got " + std::to_string(bpvSigned), Here());
    }
    bitsPerValue_ = static_cast<uint32_t>(bpvSigned);

    // Validate decimal-scale-factor fits in int32_t range
    auto dsfSigned = compConf.parsedConfig().getInt("decimal-scale-factor", 0);
    if (dsfSigned < std::numeric_limits<int32_t>::min() || dsfSigned > std::numeric_limits<int32_t>::max()) {
        throw eckit::UserError(
            "EncodeTensogram: decimal-scale-factor out of int32 range, got " + std::to_string(dsfSigned), Here());
    }
    decimalScaleFactor_ = static_cast<int32_t>(dsfSigned);

    LOG_DEBUG_LIB(LibMultio) << "EncodeTensogram: encoding=" << encoding_ << " filter=" << filter_
                             << " compression=" << compression_ << " bits-per-value=" << bitsPerValue_
                             << " decimal-scale-factor=" << decimalScaleFactor_ << " hash=" << hashAlgo_ << std::endl;
}

//----------------------------------------------------------------------------------------------------------------------
// JSON builder using eckit::JSON
//----------------------------------------------------------------------------------------------------------------------

void EncodeTensogram::writeBaseEntry(eckit::JSON& json, const message::Metadata& md) const {
    // Partition metadata into MARS keys and extra keys
    json.startObject();

    // First pass: collect and write MARS keys under a "mars" sub-object
    bool hasMars = false;
    for (const auto& [key, val] : md) {
        const std::string& keyStr = static_cast<const std::string&>(key);
        if (marsKeys.count(keyStr)) {
            if (!hasMars) {
                json << "mars";
                json.startObject();
                hasMars = true;
            }
            json << keyStr;
            emitJsonValue(json, val);
        }
    }
    if (hasMars) {
        json.endObject();
    }

    // Second pass: write non-MARS, non-skip keys at top level
    for (const auto& [key, val] : md) {
        const std::string& keyStr = static_cast<const std::string&>(key);
        if (!marsKeys.count(keyStr) && !skipKeys.count(keyStr)) {
            json << keyStr;
            emitJsonValue(json, val);
        }
    }

    json.endObject();
}

std::string EncodeTensogram::buildEncodeJson(const message::Metadata& md, size_t globalSize, const std::string& dtype,
                                             const std::string& byteOrder, size_t bytesPerElement,
                                             double referenceValue, int32_t binaryScaleFactor) const {
    std::ostringstream oss;
    {
        eckit::JSON json(oss);
        json.startObject();

        // Version
        json << "version" << 2;

        // Descriptors array (one tensor object)
        json << "descriptors";
        json.startList();
        json.startObject();
        json << "type" << "ndarray";
        json << "ndim" << 1;
        json << "shape";
        json.startList();
        json << globalSize;
        json.endList();
        json << "strides";
        json.startList();
        json << bytesPerElement;
        json.endList();
        json << "dtype" << dtype;
        json << "byte_order" << byteOrder;
        json << "encoding" << encoding_;
        json << "filter" << filter_;
        json << "compression" << compression_;

        // Szip-specific parameters
        if (compression_ == "szip") {
            json << "szip_rsi" << 32;
            json << "szip_block_size" << 8;
            json << "szip_flags" << 4;  // EC (entropy coding)
        }

        // Simple packing parameters
        if (useSimplePacking_) {
            json << "bits_per_value" << bitsPerValue_;
            json << "decimal_scale_factor" << decimalScaleFactor_;
            json.precision(17);  // Full double precision for reference_value
            json << "reference_value" << referenceValue;
            json << "binary_scale_factor" << binaryScaleFactor;
        }

        json.endObject();
        json.endList();  // end descriptors

        // Base array (per-object metadata)
        json << "base";
        json.startList();
        writeBaseEntry(json, md);
        json.endList();

        json.endObject();
    }
    return oss.str();
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

    // Extract globalSize with context-enriched error
    std::int64_t globalSizeSigned = 0;
    try {
        globalSizeSigned = msg.globalSize();
    }
    catch (const std::exception& e) {
        throw eckit::UserError(
            std::string("EncodeTensogram: cannot read globalSize from message metadata: ") + e.what(), Here());
    }

    if (globalSizeSigned <= 0) {
        throw eckit::UserError("EncodeTensogram: globalSize must be positive, got " + std::to_string(globalSizeSigned),
                               Here());
    }
    const auto globalSize = static_cast<size_t>(globalSizeSigned);

    // Determine byte order for this platform
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    const std::string byteOrder = "little";
#else
    const std::string byteOrder = "big";
#endif

    // Dispatch on precision (float32 / float64)
    auto encoded = dispatchPrecisionTag(msg.precision(), [&](auto pt) -> std::vector<uint8_t> {
        using Precision = typename decltype(pt)::type;

        // Validate payload alignment and size consistency
        if (msg.payload().size() % sizeof(Precision) != 0) {
            throw eckit::UserError("EncodeTensogram: payload size (" + std::to_string(msg.payload().size())
                                       + ") is not aligned to element size (" + std::to_string(sizeof(Precision)) + ")",
                                   Here());
        }

        const auto* values = reinterpret_cast<const Precision*>(msg.payload().data());
        const size_t numValues = msg.payload().size() / sizeof(Precision);

        if (numValues != globalSize) {
            throw eckit::UserError("EncodeTensogram: payload element count (" + std::to_string(numValues)
                                       + ") does not match globalSize (" + std::to_string(globalSize) + ")",
                                   Here());
        }

        if (useSimplePacking_) {
            // simple_packing requires float64 input — convert if needed
            std::vector<double> doubleValues;
            const double* doublePtr = nullptr;

            if constexpr (std::is_same_v<Precision, double>) {
                doublePtr = values;
            }
            else {
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

            // Build JSON and encode — input stride is always 8 (float64)
            std::string json = buildEncodeJson(md, globalSize, "float64", byteOrder, sizeof(double), referenceValue,
                                               binaryScaleFactor);

            const auto* dataPtr = reinterpret_cast<const uint8_t*>(doublePtr);
            size_t dataLen = numValues * sizeof(double);

            try {
                return tensogram::encode(json, {{dataPtr, dataLen}}, tensogram::encode_options{hashAlgo_});
            }
            catch (const std::exception& e) {
                throw eckit::SeriousBug("EncodeTensogram: tensogram::encode() failed for field with globalSize="
                                            + std::to_string(globalSize) + ", encoding=" + encoding_ + ": " + e.what(),
                                        Here());
            }
        }
        else {
            // encoding = "none" — pass raw data in native precision
            std::string dtype = std::is_same_v<Precision, double> ? "float64" : "float32";

            std::string json = buildEncodeJson(md, globalSize, dtype, byteOrder, sizeof(Precision), 0.0 /* unused */,
                                               0 /* unused */);

            const auto* dataPtr = reinterpret_cast<const uint8_t*>(values);
            size_t dataLen = numValues * sizeof(Precision);

            try {
                return tensogram::encode(json, {{dataPtr, dataLen}}, tensogram::encode_options{hashAlgo_});
            }
            catch (const std::exception& e) {
                throw eckit::SeriousBug("EncodeTensogram: tensogram::encode() failed for field with globalSize="
                                            + std::to_string(globalSize) + ", encoding=none: " + e.what(),
                                        Here());
            }
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
