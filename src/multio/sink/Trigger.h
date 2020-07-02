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

#ifndef multio_Trigger_H
#define multio_Trigger_H

#include <vector>

#include "eckit/types/Types.h"
#include "eckit/memory/NonCopyable.h"

#include "metkit/data/Message.h"

//----------------------------------------------------------------------------------------------------------------------

namespace eckit { class Configuration; }

namespace multio {


class EventTrigger;

class Trigger : public eckit::NonCopyable {

public: // methods

    Trigger(const eckit::Configuration& config);

    ~Trigger();

    void events(const eckit::StringDict& metadata) const;
    void events(metkit::data::Message msg) const;

private: // methods

    void print(std::ostream&) const;

    friend std::ostream& operator<<(std::ostream& s, const Trigger& p) { p.print(s); return s; }


private: // members


    std::vector<EventTrigger*> triggers_;

};

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio

#endif

