/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Olivier Iffrig

/// @date Nov 2019

#include <iostream>
#include <string.h>

#include "eccodes.h"

#include "eckit/exception/Exceptions.h"
#include "eckit/io/StdFile.h"
#include "eckit/log/Log.h"
#include "eckit/maths/Functions.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"
#include "eckit/runtime/Tool.h"

#include "multio/ifsio_legacy.h"

#include "multio/tools/MultioTool.h"

namespace multio {

class MultioLegacy final : public multio::MultioTool {
public:  // methods
    MultioLegacy(int argc, char** argv);

private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << std::endl << "Usage: " << tool << " [options]" << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

    void finish(const eckit::option::CmdArgs&) override;

    void execute(const eckit::option::CmdArgs& args) override;

};

MultioLegacy::MultioLegacy(int argc, char** argv) : multio::MultioTool(argc, argv) {}

void MultioLegacy::init(const eckit::option::CmdArgs& args) {}

void MultioLegacy::finish(const eckit::option::CmdArgs& args) {}

void MultioLegacy::execute(const eckit::option::CmdArgs& args) {
    eckit::AutoStdFile fin(args(0));

    int err;
    codes_handle* handle = codes_handle_new_from_file(nullptr, fin, PRODUCT_GRIB, &err);
    ASSERT(handle);

    const void* buf = nullptr;
    size_t sz = 0;
    CODES_CHECK(codes_get_message(handle, &buf, &sz), NULL);

    size_t words = eckit::round(sz, sizeof(fortint)) / sizeof(fortint);
    size_t fieldcount = 1;

    codes_keys_iterator *iter = codes_keys_iterator_new(handle, 0, "mars");

    fortint fdb_comm = 0;
    fortint fdb_addr;
    fortint fdb_fieldcount = static_cast<fortint>(fieldcount);
    fortint iwords = static_cast<fortint>(words);

    isetcommfdb_(&fdb_comm);
    iinitfdb_();
    iopenfdb_("fdb", &fdb_addr, "w", 3, 1);

    isetfieldcountfdb_(&fdb_addr, &fdb_fieldcount, &fdb_fieldcount);

    while(codes_keys_iterator_next(iter)) {
        const char *keyname = codes_keys_iterator_get_name(iter);
        char keyval[1024];
        size_t keylen = sizeof(keyval);
        CODES_CHECK(codes_get_string(handle, keyname, keyval, &keylen), NULL);
        isetvalfdb_(&fdb_addr, keyname, keyval, strlen(keyname), strlen(keyval));
    }

    iwritefdb_(&fdb_addr, buf, &iwords);
    iflushfdb_(&fdb_addr);

    iclosefdb_(&fdb_addr);

    codes_keys_iterator_delete(iter);
    codes_handle_delete(handle);
}

}  // namespace multio


//---------------------------------------------------------------------------------------------------------------


int main(int argc, char** argv) {
    multio::MultioLegacy tool(argc, argv);
    return tool.start();
}
