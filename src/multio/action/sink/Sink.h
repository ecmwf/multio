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

#pragma once

#include <iosfwd>

#include "multio/action/Action.h"
#include "multio/sink/MultIO.h"

namespace multio::action::sink {

using message::Message;

class Sink : public Action {
public:
    explicit Sink(const ComponentConfiguration& compConf);

    void executeImpl(Message msg) override;

private:
    void print(std::ostream& os) const override;

    void write(Message msg);

    void flush();

    void trigger(const Message& msg);

    multio::sink::MultIO mio_;
};

}  // namespace multio::action::sink
