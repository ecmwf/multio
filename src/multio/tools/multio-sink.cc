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

#include "eccodes.h"

#include "eckit/exception/Exceptions.h"
#include "eckit/io/StdFile.h"
#include "eckit/log/Log.h"
#include "eckit/maths/Functions.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"
#include "eckit/runtime/Tool.h"

#include "multio/ifsio.h"

namespace eckit {
class Configuration;

namespace option {
class CmdArgs;
class Option;
}  // namespace option
}  // namespace eckit

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

//---------------------------------------------------------------------------------------------------------------

MultioTool::MultioTool(int argc, char** argv) : eckit::Tool(argc, argv, "MULTIO_HOME") {}

void MultioTool::run() {
    std::function<void(const std::string&)> usage = [this](const std::string& name) {
        this->usage(name);
    };

    eckit::option::CmdArgs args(usage, options_, numberOfPositionalArguments(),
                                minimumPositionalArguments());

    init(args);
    execute(args);
    finish(args);
}

//---------------------------------------------------------------------------------------------------------------

class MultioSink final : public multio::MultioTool {
public:  // methods
    MultioSink(int argc, char** argv);

private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << std::endl << "Usage: " << tool << " [options]" << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

    void finish(const eckit::option::CmdArgs&) override;

    void execute(const eckit::option::CmdArgs& args) override;

};

MultioSink::MultioSink(int argc, char** argv) : multio::MultioTool(argc, argv) {}

void MultioSink::init(const eckit::option::CmdArgs& args) {}

void MultioSink::finish(const eckit::option::CmdArgs& args) {}

void MultioSink::execute(const eckit::option::CmdArgs& args) {
    eckit::AutoStdFile fin(args(0));

    int err;
    codes_handle* handle = codes_handle_new_from_file(nullptr, fin, PRODUCT_GRIB, &err);
    ASSERT(handle);

    const void* buf = nullptr;
    size_t sz = 0;
    CODES_CHECK(codes_get_message(handle, &buf, &sz), NULL);

    size_t words = eckit::round(sz, sizeof(fortint)) / sizeof(fortint);

    fortint iwords = static_cast<fortint>(words);

    imultio_write_(buf, &iwords);

    imultio_flush_();

    codes_handle_delete(handle);
}

}  // namespace multio


//---------------------------------------------------------------------------------------------------------------


int main(int argc, char** argv) {
    multio::MultioSink tool(argc, argv);
    return tool.start();
}
