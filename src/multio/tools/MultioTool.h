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

/// @date Oct 2019

#ifndef multio_MultioTool_H
#define multio_MultioTool_H

#include "eckit/option/CmdArgs.h"
#include "eckit/runtime/Tool.h"

namespace multio {

class MultioTool : public eckit::Tool {
public:

    virtual void usage(const std::string &tool) const = 0;

protected:

    MultioTool(int argc, char** argv);

    std::vector<eckit::option::Option*> options_;

private:
    virtual void init(const eckit::option::CmdArgs&) = 0;
    virtual void finish(const eckit::option::CmdArgs&) = 0;
    virtual void execute(const eckit::option::CmdArgs& args) = 0;

    virtual int numberOfPositionalArguments() const { return -1; }
    virtual int minimumPositionalArguments() const { return -1; }

    void run() override final;
};

} // namespace multio

#endif
