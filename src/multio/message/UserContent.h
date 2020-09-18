/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany
/// @author Simon Smart
/// @author Tiago Quintino

/// @date Sep 2020

#ifndef multio_message_UserContent_H
#define multio_message_UserContent_H

#include "eckit/message/MessageContent.h"

#include "multio/message/Metadata.h"

namespace multio {
namespace message {

class UserContent : public eckit::message::MessageContent {
public:
    UserContent(const void*, size_t size);
    UserContent(const void*, size_t size, const Metadata& metadata);

    void write(eckit::DataHandle &) const override;
    eckit::DataHandle* readHandle() const override;

    size_t length() const override;
    const void* data() const override;

    std::string getString(const std::string &key) const override;
    long getLong(const std::string &key) const override;
    double getDouble(const std::string &key) const override;

private:
    const void* data_;
    const size_t size_;

    const Metadata metadata_;

    void print(std::ostream &) const override;
};

}  // namespace message
}  // namespace multio

#endif
