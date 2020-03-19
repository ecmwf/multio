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

#include <fstream>
#include <regex>

#include "eckit/exception/Exceptions.h"
#include "eckit/io/StdFile.h"
#include "eckit/log/Log.h"
#include "eckit/maths/Functions.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"
#include "eckit/runtime/Tool.h"

#include "multio/ifsio.h"

#include "multio/tools/MultioTool.h"

namespace multio {

namespace {
class TempFile {
    const std::string path_;

public:
    TempFile(std::string&& path) : path_{std::move(path)} {}
    ~TempFile() { std::remove(path_.c_str()); }

    const std::string& path() const { return path_; }
};
}  // namespace

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

    bool subtocExists() const;

    bool testSubtoc_ = false;
};

MultioSink::MultioSink(int argc, char** argv) : multio::MultioTool(argc, argv) {
    options_.push_back(
        new eckit::option::SimpleOption<bool>("test-subtoc", "Test if subtoc has been created"));
}

void MultioSink::init(const eckit::option::CmdArgs& args) {
    args.get("test-subtoc", testSubtoc_);
}

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

    if(testSubtoc_) {
        ASSERT(subtocExists());
    }
}

bool MultioSink::subtocExists() const {
    eckit::PathName fdb_root_path{"~fdb/tests/fdb/root"};

    TempFile file{"tmp.out"};

    std::string cmd{"find " + fdb_root_path.asString() + " -name toc* > " + file.path()};
    std::system(cmd.c_str());

    const std::regex subtoc{"^toc\\.[0-9]{8}\\.[0-9]{6}\\..*", std::regex_constants::egrep};

    std::ifstream ifstrm{file.path().c_str()};
    std::string line;
    while (std::getline(ifstrm, line)) {
        auto fname = line.substr(line.rfind("/") + 1);
        if (std::regex_match(fname, subtoc)) {
            return true;
        }
    }
    return false;
}

}  // namespace multio

//---------------------------------------------------------------------------------------------------------------


int main(int argc, char** argv) {
    multio::MultioSink tool(argc, argv);
    return tool.start();
}
