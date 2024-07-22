/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Philipp Geier

/// @date Jul 2024

#pragma once


#include "multio/action/Action.h"

#include <queue>

namespace multio {

namespace action {

using message::Message;

class DebugSink : public Action {
public:
    explicit DebugSink(const ComponentConfiguration& compConf);

    void executeImpl(message::Message msg) override;

private:
    void print(std::ostream& os) const override;

    std::queue<message::Message>& debugSink_;
};

}  // namespace action
}  // namespace multio
