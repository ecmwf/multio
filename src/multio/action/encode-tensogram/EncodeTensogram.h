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

#pragma once

#include <cstdint>
#include <string>

#include "multio/action/ChainedAction.h"


namespace multio::action::encode_tensogram {

//----------------------------------------------------------------------------------------------------------------------

/// @brief Encodes raw field data into the Tensogram N-dimensional tensor message format.
///
/// This action sits in a processing pipeline and transforms Field messages containing
/// raw floating-point arrays into self-describing Tensogram binary messages. MARS metadata
/// from the input message is preserved both on the output Message (for downstream routing)
/// and inside the Tensogram message (in base[0].mars for external tools).
///
/// The encoding pipeline (simple_packing, filter, compression) is configurable via YAML.
///
/// Non-Field messages (Flush, Notification, etc.) are passed through unchanged.
///
/// YAML configuration:
/// @code
///   - type: encode-tensogram
///     encoding: simple_packing   # "none" | "simple_packing" (default: simple_packing)
///     filter: none               # "none" | "shuffle"          (default: none)
///     compression: szip          # "none"|"szip"|"zstd"|"lz4"  (default: szip)
///     bits-per-value: 16         # uint32, for simple_packing  (default: 16)
///     decimal-scale-factor: 0    # int32,  for simple_packing  (default: 0)
///     hash: xxh3                 # "xxh3" | "" to disable      (default: xxh3)
/// @endcode

class EncodeTensogram : public ChainedAction {
public:
    explicit EncodeTensogram(const ComponentConfiguration& compConf);

    void executeImpl(message::Message msg) override;

private:
    void print(std::ostream& os) const override;

    /// Build the Tensogram metadata+descriptors JSON from multio metadata and field dimensions.
    /// The JSON follows the tensogram C API contract:
    ///   { "version": 2, "descriptors": [...], "base": [{"mars": {...}, ...}] }
    std::string buildEncodeJson(const multio::message::Metadata& md, size_t globalSize, const std::string& dtype,
                                const std::string& byteOrder, double referenceValue, int32_t binaryScaleFactor) const;

    /// Build the Tensogram metadata+descriptors JSON for unencoded (raw) data.
    std::string buildEncodeJsonRaw(const multio::message::Metadata& md, size_t globalSize, const std::string& dtype,
                                   const std::string& byteOrder, size_t bytesPerElement) const;

    // --- Configuration from YAML ---
    std::string encoding_;        ///< "none" | "simple_packing"
    std::string filter_;          ///< "none" | "shuffle"
    std::string compression_;     ///< "none" | "szip" | "zstd" | "lz4"
    std::string hashAlgo_;        ///< "xxh3" | ""
    uint32_t bitsPerValue_;       ///< For simple_packing (default: 16)
    int32_t decimalScaleFactor_;  ///< For simple_packing (default: 0)
};

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio::action::encode_tensogram
