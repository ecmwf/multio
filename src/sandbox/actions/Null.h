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

#ifndef multio_sandbox_PrintAction_H
#define multio_sandbox_PrintAction_H

#include "sandbox/Action.h"


namespace eckit { class Configuration; }

namespace multio {
namespace sandbox {
namespace actions {

class Null : public Action {
public:
    Null(const eckit::Configuration& config);


protected: // methods

    virtual void execute(Message msg);

private: // methods

    virtual void print(std::ostream &os) const;

};

}  // namespace actions
}  // namespace sandbox
}  // namespace multio

#endif
