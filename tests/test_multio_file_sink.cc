/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "TestHelpers.h"

#include <cstring>
#include <unistd.h>

#include "eckit/testing/Test.h"
#include "eckit/filesystem/TmpFile.h"
#include "eckit/message/Message.h"

#include "multio/message/UserContent.h"
#include "multio/sink/FileSink.h"

namespace multio {
namespace test {

CASE("FileSink exists in factory") {
    // DataSinkFactory::list appends the results to a ostream&, so we need to extract them.
    std::stringstream ss;
    DataSinkFactory::instance().list(ss);
    EXPECT(ss.str().find("file") != std::string::npos);
}

CASE("FileSink is created successfully") {
    const eckit::PathName& file_path = eckit::TmpFile();
    auto sink = make_configured_file_sink(file_path);
    auto fileSink = dynamic_cast<FileSink*>(sink.get());
    EXPECT(fileSink);
    EXPECT(file_path.exists());
}

CASE("FileSink writes correctly") {
    const eckit::PathName& file_path = eckit::TmpFile();
    auto sink = make_configured_file_sink(file_path);
    const char quote[] =
        "All was quiet in the deep dark wood. The mouse found a nut and the nut was good.";

    eckit::message::Message msg{new message::UserContent{quote, sizeof(quote) - 1}};
    sink->write(msg);

    EXPECT(file_content(file_path) == std::string(quote));
}

CASE("FileSink creates new file by default") {
    const eckit::PathName& file_path = eckit::TmpFile();
    const char quote[] =
        "All was quiet in the deep dark wood. The mouse found a nut and the nut was good.";

    {
        auto sink = make_configured_file_sink(file_path);
        eckit::message::Message msg{new message::UserContent{quote, sizeof(quote) - 1}};
        sink->write(msg);
    }

    {
        auto sink = make_configured_file_sink(file_path);
        eckit::message::Message msg{new message::UserContent{quote, sizeof(quote) - 1}};
        sink->write(msg);
    }

    EXPECT(file_content(file_path) == std::string(quote));
}


CASE("FileSink creates new file by explicit request") {
    const eckit::PathName& file_path = eckit::TmpFile();
    const char quote[] =
        "All was quiet in the deep dark wood. The mouse found a nut and the nut was good.";

    {
        auto sink = make_configured_file_sink(file_path);
        eckit::message::Message msg{new message::UserContent{quote, sizeof(quote) - 1}};
        sink->write(msg);
    }

    {
        auto sink = make_configured_file_sink(file_path, false);
        eckit::message::Message msg{new message::UserContent{quote, sizeof(quote) - 1}};
        sink->write(msg);
    }

    EXPECT(file_content(file_path) == std::string(quote));
}

CASE("FileSink appends to existing file") {
    const eckit::PathName& file_path = eckit::TmpFile();
    const char quote[] =
        "All was quiet in the deep dark wood. The mouse found a nut and the nut was good.";

    {
        auto sink = make_configured_file_sink(file_path);
        eckit::message::Message msg{new message::UserContent{quote, sizeof(quote) - 1}};
        sink->write(msg);
    }

    {
        auto sink = make_configured_file_sink(file_path, true);
        eckit::message::Message msg{new message::UserContent{quote, sizeof(quote) - 1}};
        sink->write(msg);
    }

    EXPECT(file_content(file_path) == std::string{quote} + std::string{quote});
}

}  // namespace test
}  // namespace multio

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}
