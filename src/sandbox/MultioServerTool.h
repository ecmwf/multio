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

#ifndef multio_sandbox_MultioServerTool_H
#define multio_sandbox_MultioServerTool_H

#include "eckit/option/SimpleOption.h"
#include "eckit/runtime/Tool.h"

namespace eckit {
    class Configuration;
    namespace option {
        class CmdArgs;
    }
}

namespace multio {
namespace sandbox {

class MultioServerTool : public eckit::Tool {
protected:  // methods

    MultioServerTool(int argc, char** argv);
    ~MultioServerTool() override;

public:  // methods

    virtual void usage(const std::string &tool) const = 0;

protected:  // members
    std::vector<eckit::option::Option*> options_;

protected:  // methods
    virtual void init(const eckit::option::CmdArgs& args);
    virtual void finish(const eckit::option::CmdArgs&);

private:  // methods
    virtual void execute(const eckit::option::CmdArgs& args) = 0;

    virtual int numberOfPositionalArguments() const { return -1; }
    virtual int minimumPositionalArguments() const { return -1; }

    void run() override;

protected:  // members
    size_t nbServers_ = 1;
};


}  // namespace sandbox
}  // namespace multio

#endif
