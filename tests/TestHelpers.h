/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef multio_TestHelpers_H
#define multio_TestHelpers_H

#include "eckit/filesystem/TmpFile.h"
#include "eckit/config/LocalConfiguration.h"

#include "multio/FileSink.h"

#include "TestDataBlob.h"

namespace multio {
namespace test {

class TestFile {
    const eckit::PathName name_;

public:
    TestFile(eckit::PathName&& nm) : name_(std::move(nm)) { name_.touch(); }
    ~TestFile() { name_.unlink(); }
    eckit::PathName const& name() const { return name_; }
};


// Helpers for file-sink tests
inline auto make_configured_file_sink(const eckit::PathName& file_path)
    -> std::unique_ptr<DataSink> {
    eckit::LocalConfiguration config;
    config.set("path", file_path);
    return std::unique_ptr<DataSink>(DataSinkFactory::instance().build("file", config));
}

inline auto file_content(const eckit::PathName& file_path) -> std::string {
    std::fstream ifs(std::string(file_path.fullName()).c_str());
    return std::string(std::istreambuf_iterator<char>(ifs), std::istreambuf_iterator<char>());
}

}  // namespace test
}  // namespace multio

#endif  // multio_TestHelpers_H
