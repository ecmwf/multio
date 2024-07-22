/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "DebugSink.h"

#include <iostream>
#include <sstream>

#include "eckit/exception/Exceptions.h"
#include "eckit/message/Message.h"

#include "multio/LibMultio.h"
#include "multio/message/Glossary.h"
#include "multio/sink/DataSink.h"

namespace multio::action {

using message::glossary;

DebugSink::DebugSink(const ComponentConfiguration& compConf) :
    Action{compConf}, debugSink_{compConf.multioConfig().debugSink()} {}

void DebugSink::executeImpl(Message msg) {
    debugSink_.push(std::move(msg));
}

void DebugSink::print(std::ostream& os) const {
    os << "DebugSink()";
}

static ActionBuilder<DebugSink> DebugSinkBuilder("debug-sink");

}  // namespace multio::action
