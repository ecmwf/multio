/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Transport.h"

namespace multio {
namespace action {

Transport::Transport(const eckit::Configuration& config) :
    Action{config} {}

void Transport::execute(Message msg) const {
    if(buffered_) {
        transport_->send(msg);
    } else {
        transport_->sendBuffered();
    }
}

void Transport::print(std::ostream& os) const {
    os << "Action[" << *transport_ os << "]";
}


static ActionBuilder<Transport> TransportBuilder("Transport");

}  // namespace action
}  // namespace multio
