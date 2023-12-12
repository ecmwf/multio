/*
 * (C) Copyright 1996-2015 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <fstream>

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/log/JSON.h"
#include "eckit/testing/Test.h"
#include "eckit/utils/Translator.h"

#include "multio/config/ComponentConfiguration.h"
#include "multio/sink/FileSink.h"
#include "multio/sink/MultIO.h"

#include "TestDataContent.h"
#include "TestHelpers.h"

namespace multio {
namespace test {

namespace {

std::string create_test_configuration(const eckit::PathName& file1, const eckit::PathName& file2,
                                      const eckit::PathName& file3, int jobId, int port) {
    std::stringstream ss;
    ss << R"json(
          {
            "triggers" : [
              { "type" : "MetadataChange",
                "host" : "localhost",
                "port" : )json"
       << port << R"json(,
                "retries" : 0,
                "timeout" : 1,
                "key" : "step",
                "values" : [0, 3, 6, 9, 12, 24],
                "info" : { "job" : )json"
       << jobId << R"json(,
                           "job_name" : "epsnemo"
                }
              },
              { "type" : "MetadataChange",
                "file" : ")json"
       << file1.baseName() << R"json(",
                "key" : "step",
                "values" : [0, 3, 6, 9, 12, 24],
                "info" : { "job" : )json"
       << jobId << R"json(,
                           "job_name" : "epsnemo"
                }
              },
              { "type" : "MetadataChange",
                "file" : ")json"
       << file2.baseName() << R"json(",
                "key" : "step",
                "values" : [1, 4, 5, 6, 10],
                "info" : { "job" : )json"
       << jobId << R"json(,
                           "job_name" : "epsnemo"
                }
              }
            ]
          }
          )json";

    return ss.str();
}

std::string expected_file_content(const std::vector<int> steps_to_issue, const int jobId) {
    std::ostringstream oss;
    for (std::vector<int>::const_iterator it = steps_to_issue.begin(); it != steps_to_issue.end(); ++it) {
        oss << R"json({"type":"MetadataChange","info":{"app":"multio","job":")json" << jobId
            << R"json(","job_name":"epsnemo"},"metadata":{"step":")json" << *it << R"json("}})json" << std::endl;
    }
    return oss.str();
}

bool trigger_executed_correctly(const eckit::PathName& file, const std::vector<int>& steps_to_issue, const int jobId) {
    EXPECT(file.exists());

    std::string expected = expected_file_content(steps_to_issue, jobId);

    std::fstream ifs(std::string(file.baseName()).c_str());
    std::string actual((std::istreambuf_iterator<char>(ifs)), (std::istreambuf_iterator<char>()));

    return (expected == actual);
}

}  // namespace

//-----------------------------------------------------------------------------


CASE("test_multio_with_event_trigger") {
    TestFile file1{eckit::TmpFile().baseName()};
    TestFile file2{eckit::TmpFile().baseName()};
    TestFile file3{eckit::TmpFile().baseName()};

    // Set up
    int port = 10000;
    int jobId = 234;

    std::string tconf = create_test_configuration(file1.name(), file2.name(), file3.name(), jobId, port);
    ::setenv("MULTIO_CONFIG_TRIGGERS", tconf.c_str(), 1);

    std::string sinks(R"json({
                  "sinks" : [ {
                      "type" : "file",
                      "path" : ")json"
                      + file3.name() + R"json("
                    } ]
                }
                )json");

    eckit::LocalConfiguration config{eckit::YAMLConfiguration(sinks)};
    config::MultioConfiguration multioConf(config);
    ComponentConfiguration compConf(config, multioConf);
    {
        auto mio = std::make_unique<MultIO>(compConf);

        for (int step = 0; step <= 24; step++) {
            for (int level = 1; level <= 5; level++) {
                std::ostringstream os;
                eckit::JSON json(os);

                auto stream = "oper";
                [step, level, stream](eckit::JSON& json) {
                    json.startObject();
                    json << "stream" << stream;
                    json << "step" << eckit::Translator<long, std::string>{}(step);
                    json << "level" << eckit::Translator<long, std::string>{}(level);
                    json.endObject();
                }(json);

                eckit::Log::info() << "JSON content: " << os.str() << std::endl;

                message::Metadata md{eckit::YAMLConfiguration{os.str()}};
                eckit::message::Message msg{new TestDataContent{os.str().c_str(), os.str().length(), md}};

                mio->write(msg);
            }
        }
    }  // mio is destroyed

    // First list of triggers
    {
        const int arr_sz = 6;
        int arr[arr_sz] = {0, 3, 6, 9, 12, 24};
        std::vector<int> steps_to_issue(&arr[0], &arr[arr_sz]);
        EXPECT(trigger_executed_correctly(file1.name(), steps_to_issue, jobId));
    }

    // Second list of triggers
    {
        const int arr_sz = 5;
        int arr[arr_sz] = {1, 4, 5, 6, 10};
        std::vector<int> steps_to_issue(&arr[0], &arr[arr_sz]);
        EXPECT(trigger_executed_correctly(file2.name(), steps_to_issue, jobId));
    }
}

//-----------------------------------------------------------------------------

}  // namespace test
}  // namespace multio

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}
