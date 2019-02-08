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

#include <memory>
#include <string>

#include "sandbox/Action.h"

namespace eckit {
    class Configuration;
    class DataHandle;
}

namespace multio {
namespace sandbox {
namespace actions {

class AppendToFile : public Action {
public: // methods
    AppendToFile(const eckit::Configuration& config);

    ~AppendToFile();

protected: // methods

    virtual void execute(Message msg);

private: // methods

    virtual void print(std::ostream &os) const;

private: // members

    std::string path_;

    std::unique_ptr<eckit::DataHandle> datahandle_;
};

}  // namespace actions
}  // namespace sandbox
}  // namespace multio

#endif
