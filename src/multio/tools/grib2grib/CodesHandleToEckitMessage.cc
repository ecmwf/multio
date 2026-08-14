/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

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

class OwningBufferContent : public eckit::message::MessageContent {
public:
    explicit OwningBufferContent(eckit::Buffer&& buffer) : buffer_(std::move(buffer)) {}

private:
    eckit::Buffer buffer_;

    eckit::DataHandle* readHandle() const override { return new eckit::MemoryHandle(buffer_.data(), buffer_.size()); }
    size_t length() const override { return buffer_.size(); }
    const void* data() const override { return buffer_.data(); }
    void write(eckit::DataHandle& handle) const override {
        if (handle.write(buffer_.data(), buffer_.size()) != buffer_.size()) {
            std::ostringstream oss;
            oss << "Write error to data handle " << handle;
            throw eckit::WriteError(oss.str(), Here());
        }
    }
    void print(std::ostream& s) const override { s << "OwningBufferContent[]"; }
};

}  // namespace

eckit::message::Message to_eckit_message(const metkit::codes::CodesHandle& handle) {
    eckit::Buffer buf{handle.messageSize()};
    handle.copyInto(reinterpret_cast<uint8_t*>(buf.data()), buf.size());
    return eckit::message::Message{new OwningBufferContent(std::move(buf))};
}

}  // namespace multio::distGrib1ToGrib2::grib2grib
