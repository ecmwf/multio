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

#include "multio/tools/MultioTool.h"

namespace multio {

MultioTool::MultioTool(int argc, char** argv) : eckit::Tool(argc, argv, "MULTIO_HOME") {}

void MultioTool::run() {
    std::function<void(const std::string&)> usage = [this](const std::string& name) { this->usage(name); };

    eckit::option::CmdArgs args(usage, options_, numberOfPositionalArguments(), minimumPositionalArguments());

    init(args);
    execute(args);
    finish(args);
}

}  // namespace multio
