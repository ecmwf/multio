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

#ifndef multio_sandbox_Transport_H
#define multio_sandbox_Transport_H

#include <iostream>
#include <string>

#include "eckit/memory/NonCopyable.h"
#include "eckit/config/LocalConfiguration.h"

#include "sandbox/Message.h"

namespace multio {
namespace sandbox {

template <typename ValueType>
ValueType configure_value(const eckit::Configuration& config, std::string value_name) {
    ValueType val;
    config.get(value_name, val);
    return val;
}


class Transport {
public:  // methods

    Transport(const eckit::Configuration& config);
    virtual ~Transport();

    virtual Message receive() = 0;

    virtual void send(const Message& message) = 0;

    virtual Peer localPeer() const = 0;

protected:
    std::string name_;

private: // methods

    virtual void print(std::ostream &os) const = 0;

    friend std::ostream& operator<<(std::ostream& os, const Transport& transport) {
        transport.print(os);
        return os;
    }
};

}  // namespace sandbox
}  // namespace multio

#endif
