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

/// @date Jan 2022

#ifndef multio_server_actions_Transport_H
#define multio_server_actions_Transport_H

#include "multio/action/Action.h"
#include "multio/server/Transport.h"

namespace eckit { class Configuration; }

namespace multio {
namespace action {

using message::Message;

class Transport : public Action {
public:
    explicit Transport(const eckit::Configuration& config);

    void execute(Message msg) const override;

private:
    void print(std::ostream &os) const override;

    std::shared_ptr<server::Transport> transport_ = nullptr;
    bool buffered_ = false;
};

}  // namespace action
}  // namespace multio

#endif
