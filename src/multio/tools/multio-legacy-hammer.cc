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
#include <string>
#include <string.h>
#include <unordered_set>

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

// This list is currently sufficient to get to nparams=200 of levtype=ml,type=fc
const std::unordered_set<size_t> AWKWARD_PARAMS {11, 12, 13, 14, 15, 16, 49, 51, 52, 61, 121, 122, 146, 147, 169, 175, 176, 177, 179, 189, 201, 202};

namespace multio {

class MultioLegacyHammer final : public multio::MultioTool {
public:  // methods
    MultioLegacyHammer(int argc, char** argv);

private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << std::endl << "Usage: " << tool << " [options]" << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

    void finish(const eckit::option::CmdArgs&) override;

    void execute(const eckit::option::CmdArgs& args) override;

};

MultioLegacyHammer::MultioLegacyHammer(int argc, char** argv) :
    multio::MultioTool(argc, argv) {

    options_.push_back(new eckit::option::SimpleOption<std::string>("class", "Reset class on data"));
    options_.push_back(new eckit::option::SimpleOption<std::string>("expver", "Reset expver on data"));
    options_.push_back(new eckit::option::SimpleOption<long>("nensembles", "Number of ensemble members"));
    options_.push_back(new eckit::option::SimpleOption<long>("number", "The first ensemble number to use"));
    options_.push_back(new eckit::option::SimpleOption<long>("nsteps", "Number of steps"));
    options_.push_back(new eckit::option::SimpleOption<long>("nlevels", "Number of levels"));
    options_.push_back(new eckit::option::SimpleOption<long>("nparams", "Number of parameters"));
    options_.push_back(new eckit::option::SimpleOption<bool>("notify", "Notify when a step is written"));
}

void MultioLegacyHammer::init(const eckit::option::CmdArgs& args) {
    ASSERT(args.has("class"));
    ASSERT(args.has("expver"));
    ASSERT(args.has("nsteps"));
    ASSERT(args.has("nlevels"));
    ASSERT(args.has("nparams"));
}

void MultioLegacyHammer::finish(const eckit::option::CmdArgs& args) {}

void MultioLegacyHammer::execute(const eckit::option::CmdArgs& args) {
    eckit::AutoStdFile fin(args(0));

    int err;
    codes_handle* handle = codes_handle_new_from_file(nullptr, fin, PRODUCT_GRIB, &err);
    ASSERT(handle);

    size_t nensembles = args.getLong("nensembles", 1);
    size_t number  = args.getLong("number", 1);
    size_t nsteps = args.getLong("nsteps");
    size_t nlevels = args.getLong("nlevels");
    size_t nparams = args.getLong("nparams");

    size_t size = 0;
    std::string cls = args.getString("class");
    size = cls.length();
    CODES_CHECK(codes_set_string(handle, "class", cls.c_str(), &size), 0);
    std::string expver = args.getString("expver");
    size = expver.length();
    CODES_CHECK(codes_set_string(handle, "expver", expver.c_str(), &size), 0);

    const void* buf = nullptr;
    size_t sz = 0;
    CODES_CHECK(codes_get_message(handle, &buf, &sz), NULL);

    size_t words = eckit::round(sz, sizeof(fortint)) / sizeof(fortint);
    size_t fieldcount = nensembles * nsteps * nlevels * nparams;

    fortint fdb_comm = 0;
    fortint fdb_addr;
    fortint fdb_fieldcount = static_cast<fortint>(fieldcount);
    fortint iwords = static_cast<fortint>(words);

    isetcommfdb_(&fdb_comm);
    iinitfdb_();
    iopenfdb_("fdb", &fdb_addr, "w", 3, 1);

    isetfieldcountfdb_(&fdb_addr, &fdb_fieldcount, &fdb_fieldcount);

    std::string keyname;
    codes_keys_iterator *iter = codes_keys_iterator_new(handle, 0, "mars");
    while(codes_keys_iterator_next(iter)) {
        keyname = codes_keys_iterator_get_name(iter);
        char keyval[1024];
        size_t keylen = sizeof(keyval);
        CODES_CHECK(codes_get_string(handle, keyname.c_str(), keyval, &keylen), NULL);
        isetvalfdb_(&fdb_addr, keyname.c_str(), keyval, keyname.length(), strlen(keyval));
    }

    bool notify = args.getBool("notify", false);
    std::string keyval;
    eckit::Translator<size_t, std::string> to_string;
    for (size_t member = 0; member < nensembles; ++member) {
        if (args.has("nensembles")) {
            keyname = "number";
            keyval = to_string(member + number);
            CODES_CHECK(codes_set_long(handle, keyname.c_str(), member + number), 0);
            isetvalfdb_(&fdb_addr, keyname.c_str(), keyval.c_str(), keyname.length(), keyval.length());
        }

        for (size_t step = 0; step < nsteps; ++step) {
            keyname = "step";
            keyval = to_string(step);
            CODES_CHECK(codes_set_long(handle, keyname.c_str(), step), 0);
            isetvalfdb_(&fdb_addr, keyname.c_str(), keyval.c_str(), keyname.length(), keyval.length());

            for (size_t level = 1; level <= nlevels; ++level) {
                keyname = "level";
                keyval = to_string(level);
                CODES_CHECK(codes_set_long(handle, keyname.c_str(), level), 0);
                isetvalfdb_(&fdb_addr, keyname.c_str(), keyval.c_str(), keyname.length(), keyval.length());

                for (size_t param = 1, real_param = 1; param <= nparams; ++param, ++real_param) {
                    // GRIB API only allows us to use certain parameters
                    while (AWKWARD_PARAMS.find(real_param) != AWKWARD_PARAMS.end()) {
                        real_param++;
                    }

                    keyname = "param";
                    keyval = to_string(real_param);
                    CODES_CHECK(codes_set_long(handle, keyname.c_str(), real_param), 0);
                    isetvalfdb_(&fdb_addr, keyname.c_str(), keyval.c_str(), keyname.length(), keyval.length());

                    iwritefdb_(&fdb_addr, buf, &iwords);
                }
            }

            iflushfdb_(&fdb_addr);

            if (notify) {
                fortint istep = static_cast<fortint>(step);
                imultio_notify_step_(&istep);
            }
        }
    }

    iclosefdb_(&fdb_addr);

    codes_keys_iterator_delete(iter);
    codes_handle_delete(handle);
}

}  // namespace multio


//---------------------------------------------------------------------------------------------------------------


int main(int argc, char** argv) {
    multio::MultioLegacyHammer tool(argc, argv);
    return tool.start();
}
