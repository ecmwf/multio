/*
 * (C) Copyright 1996-2015 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Tiago Quintino
/// @author Domokos Sarmany
/// @date   May 2018

#pragma once

#include <vector>

#include "eckit/memory/NonCopyable.h"
#include "eckit/types/Types.h"

#include "eckit/message/Message.h"
#include "multio/config/ComponentConfiguration.h"

//----------------------------------------------------------------------------------------------------------------------

namespace eckit {
class Configuration;
}

namespace multio::sink {

using config::ComponentConfiguration;


class EventTrigger;

class Trigger : public eckit::NonCopyable {

public:  // methods
    Trigger(const ComponentConfiguration& compConf);

    ~Trigger();

    void events(const eckit::StringDict& metadata) const;
    void events(eckit::message::Message message) const;

private:  // methods
    void print(std::ostream&) const;

    friend std::ostream& operator<<(std::ostream& s, const Trigger& p) {
        p.print(s);
        return s;
    }


private:  // members
    std::vector<std::unique_ptr<EventTrigger>> triggers_;
};

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio::sink
