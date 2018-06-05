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
#include <iostream>
#include <string>

#include "eckit/io/DataBlob.h"
#include "eckit/io/DataHandle.h"
#include "eckit/memory/ScopedPtr.h"
#include "eckit/parser/Tokenizer.h"
#include "eckit/types/Metadata.h"

#include "eckit/testing/Test.h"

namespace multio {
namespace test {

auto make_configured_file_sink(const eckit::PathName& file_path) -> std::unique_ptr<DataSink> {
    eckit::LocalConfiguration config;
    config.set("path", file_path);
    // NOTE: std::make_unique would be nicer but it doesn't work with the DataSinkFactory
    return std::unique_ptr<DataSink>(DataSinkFactory::build("file", config));
}

auto file_content(const eckit::PathName& file_path) -> std::string {
    std::fstream ifs(std::string(file_path.fullName()).c_str());
    return std::string(std::istreambuf_iterator<char>(ifs), std::istreambuf_iterator<char>());
}

// A trivial do-nothing metadata object
class TestMetadata : public eckit::Metadata {
public:  // methods
    TestMetadata() {}
    std::vector<std::string> keywords() const final { NOTIMP; };
    bool has(const std::string& name) const final { NOTIMP; }
    void get(const std::string& name, std::string& value) const final { NOTIMP; }
    void get(const std::string& name, long& value) const final { NOTIMP; }
    void get(const std::string& name, double& value) const final { NOTIMP; }
    friend std::ostream& operator<<(std::ostream& s, const TestMetadata& p) {
        p.print(s);
        return s;
    }

protected:  // methods
    virtual void print(std::ostream& os) const final { os << "TestMetadata()"; }
};

//
// A null datablob for testing the factories
class TestDataBlob : public eckit::DataBlob {
public:  // methods
    TestDataBlob(const void* data, size_t length) : DataBlob(data, length) {}
    TestDataBlob(eckit::DataHandle& dh, size_t length) : DataBlob(dh, length) {}

    const eckit::Metadata& metadata() const final { return metadata_; }

private:  // methods
    virtual void print(std::ostream& os) const { os << "TestDataBlob()"; }

private:  // members
    TestMetadata metadata_;
};

eckit::DataBlobBuilder<TestDataBlob> dbBuilder("test_blob");

}  // namespace test
}  // namespace multio
