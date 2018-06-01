#include <unistd.h>
#include <cstring>

#include "eckit/testing/Test.h"
#include "eckit/filesystem/TmpFile.h"
#include "multio/DataSink.h"
#include "multio/FileSink.h"

#include "TestHelpers.h"

using namespace eckit;
using namespace eckit::testing;

namespace multio {
namespace test {

namespace {
}

CASE("test_multio_journal") {
    eckit::PathName file_path = eckit::TmpFile();
    eckit::LocalConfiguration config;
    config.set("path", file_path);
    std::unique_ptr<DataSink> sink(DataSinkFactory::build("file", config));
    Journal journal_no_sink(config);
    Journal journal_file_sink(config, sink.get());
}

//-----------------------------------------------------------------------------

}  // namespace test
}  // namespace multio

int main(int argc, char** argv) {
    return run_tests(argc, argv);
}
