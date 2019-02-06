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

/// @date Jan 2019

#ifndef multio_sandbox_Message_H
#define multio_sandbox_Message_H

#include <string>
#include <thread>
#include <vector>

#include "sandbox/Peer.h"

namespace multio {
namespace sandbox {

class Message {
public:  // types

    enum class Tag
    {
        mapping_data,
        field_data,
        step_complete,
        forecast_complete,
        open,
        close
    };

public:  // methods
    static int protocolVersion();

    Message(Tag tag, Peer from, Peer to, const std::vector<char>& payload);

    const void* payload() const;

    size_t size() const;

    Tag tag() const { return tag_; }
    Peer to() const { return to_; }
    Peer from() const { return from_; }

    int version() const { return version_; }

private:  // methods
    void print(std::ostream& out) const;

    friend std::ostream& operator<<(std::ostream& s, const Message& x) {
        x.print(s);
        return s;
    }

private:  // members
    Tag tag_;
    int version_;

    Peer from_;
    Peer to_;

    std::vector<char> payload_;
};

}  // namespace sandbox
}  // namespace multio

#endif
