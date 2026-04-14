/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @file Parser.h
/// @brief Metadata parser for multio messages.
///
/// This parser provides the same functionality for message::Metadata as the config parser
/// (util/config/Parser.h) does for eckit::LocalConfiguration. Each action defines a plain
/// struct with a static constexpr fields_ tuple describing the metadata keys it needs:
///
/// @code
///   namespace md = multio::datamod;
///
///   struct ScaleMetadata {
///       Param param;
///       std::optional<double> missingValue;
///       static constexpr auto fields_ = std::make_tuple(
///           md::requiredEntry("param", &ScaleMetadata::param),
///           md::optionalEntry("missingValue", &ScaleMetadata::missingValue)
///       );
///   };
///
///   // In action::executeImpl:
///   auto md = md::readMetadata<ScaleMetadata>(msg.metadata());
///   md.param = Param(newId);
///   md::writeMetadata(md, msg.modifyMetadata());
/// @endcode

#pragma once

#include "eckit/config/LocalConfiguration.h"

#include "multio/message/Metadata.h"
#include "multio/util/record/Entry.h"

#include "detail/Parser.h"


namespace multio::datamod {

// Re-export shared record entry factories
using multio::util::record::optionalEntry;
using multio::util::record::requiredEntry;

/**
 * Parse a metadata struct from a message::Metadata container.
 * Required fields throw eckit::UserError if missing.
 * Optional fields use their default-initialized values when absent.
 *
 * Unlike the config parser, this does NOT validate for unknown keys --
 * metadata flows through the pipeline and contains keys for many different actions.
 */
template <typename TStruct>
TStruct readMetadata(const message::Metadata& md) {
    return detail::readMetadata<TStruct>(md);
}

/**
 * Write a metadata struct into a message::Metadata container.
 * For std::optional fields, only writes if the optional has a value.
 */
template <typename TStruct>
void writeMetadata(const TStruct& source, message::Metadata& md) {
    detail::writeMetadata(source, md);
}

/**
 * Create a new message::Metadata from a metadata struct.
 */
template <typename TStruct>
message::Metadata writeMetadata(const TStruct& source) {
    return detail::writeMetadata(source);
}

/**
 * Write a metadata struct into an eckit::LocalConfiguration.
 * For std::optional fields, only writes if the optional has a value.
 *
 * @param stripPrefix  If non-empty, strip this prefix from each key name before writing.
 *                     E.g., pass "misc-" to write MiscRecord keys without the scope prefix.
 */
template <typename TStruct>
void writeConfig(const TStruct& source, eckit::LocalConfiguration& conf, std::string_view stripPrefix = "") {
    detail::writeConfig(source, conf, stripPrefix);
}

/**
 * Create a new eckit::LocalConfiguration from a metadata struct.
 *
 * @param stripPrefix  If non-empty, strip this prefix from each key name before writing.
 */
template <typename TStruct>
eckit::LocalConfiguration writeConfig(const TStruct& source, std::string_view stripPrefix = "") {
    return detail::writeConfig(source, stripPrefix);
}


}  // namespace multio::datamod
