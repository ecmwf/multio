/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/// @file
/// @brief Conversion bridge from `metkit::codes::CodesHandle` to `eckit::message::Message`.

#include "multio/tools/grib2grib/CodesHandleToEckitMessage.h"

#include <sstream>

#include "eckit/exception/Exceptions.h"
#include "eckit/io/Buffer.h"
#include "eckit/io/DataHandle.h"
#include "eckit/io/MemoryHandle.h"
#include "eckit/message/MessageContent.h"
#include "metkit/codes/api/CodesAPI.h"

namespace multio::distGrib1ToGrib2::grib2grib {

namespace {

/// @brief Message-content implementation that owns a copied GRIB buffer.
///
/// The distributed pipeline often needs an `eckit::message::Message` even though
/// the source data currently lives inside a `metkit::codes::CodesHandle`. This
/// helper stores an owning `eckit::Buffer` so the returned message remains valid
/// after the source `CodesHandle` goes out of scope.
class OwningBufferContent : public eckit::message::MessageContent {
public:
    /// @brief Construct the message content around an already-owned buffer.
    /// @param buffer Encoded GRIB payload whose ownership is transferred into this object.
    explicit OwningBufferContent(eckit::Buffer&& buffer) : buffer_(std::move(buffer)) {}

private:
    eckit::Buffer buffer_;

    /// @brief Create a read handle over the owned in-memory payload.
    /// @return Heap-allocated memory handle owned by the caller.
    eckit::DataHandle* readHandle() const override { return new eckit::MemoryHandle(buffer_.data(), buffer_.size()); }

    /// @brief Report the encoded message size.
    /// @return Number of bytes stored in the owned payload.
    size_t length() const override { return buffer_.size(); }

    /// @brief Expose a direct pointer to the owned payload bytes.
    /// @return Raw pointer to the beginning of the encoded message.
    const void* data() const override { return buffer_.data(); }

    /// @brief Write the owned payload into an arbitrary eckit data handle.
    /// @param handle Destination handle receiving the encoded message bytes.
    /// @throw eckit::WriteError If the destination handle reports a short write.
    void write(eckit::DataHandle& handle) const override {
        if (handle.write(buffer_.data(), buffer_.size()) != buffer_.size()) {
            std::ostringstream oss;
            oss << "Write error to data handle " << handle;
            throw eckit::WriteError(oss.str(), Here());
        }
    }

    /// @brief Render a short debug representation for logs and diagnostics.
    /// @param s Output stream receiving the textual representation.
    void print(std::ostream& s) const override { s << "OwningBufferContent[]"; }
};

}  // namespace

/// @brief Copy one GRIB message out of a `CodesHandle` into an owning eckit message.
/// @param handle Read-only ecCodes wrapper whose current encoded message will be copied.
/// @return Owning `eckit::message::Message` whose payload remains valid independently of `handle`.
/// @throw eckit exception If the message cannot be copied into the destination buffer.
eckit::message::Message to_eckit_message(const metkit::codes::CodesHandle& handle) {
    eckit::Buffer buf{handle.messageSize()};
    handle.copyInto(reinterpret_cast<uint8_t*>(buf.data()), buf.size());
    return eckit::message::Message{new OwningBufferContent(std::move(buf))};
}

}  // namespace multio::distGrib1ToGrib2::grib2grib
