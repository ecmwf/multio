/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#pragma once

#include <cmath>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <string>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/filesystem/TmpFile.h"

#include "multio/sink/FileSink.h"
#include "multio/config/ComponentConfiguration.h"

namespace multio {
namespace test {

using namespace multio::sink;

class TestFile {
    const eckit::PathName name_;

public:
    TestFile(eckit::PathName&& nm) : name_(std::move(nm)) { name_.touch(); }
    ~TestFile() { name_.unlink(); }
    eckit::PathName const& name() const { return name_; }
};


// Helpers for file-sink tests
inline auto make_configured_file_sink(const eckit::PathName& file_path) -> std::unique_ptr<DataSink> {
    eckit::LocalConfiguration config;
    config.set("path", file_path);
    config::ComponentConfiguration compConf(config, "", "");
    return std::unique_ptr<DataSink>(DataSinkFactory::instance().build("file", compConf));
}

inline auto make_configured_file_sink(const eckit::PathName& file_path, bool append) -> std::unique_ptr<DataSink> {
    eckit::LocalConfiguration config;
    config.set("path", file_path);
    config.set("append", append);
    config::ComponentConfiguration compConf(config, "", "");
    return std::unique_ptr<DataSink>(DataSinkFactory::instance().build("file", compConf));
}

inline auto file_content(const eckit::PathName& file_path) -> std::string {
    std::fstream ifs(std::string(file_path.fullName()).c_str());
    return std::string(std::istreambuf_iterator<char>(ifs), std::istreambuf_iterator<char>());
}

}  // namespace test
}  // namespace multio
