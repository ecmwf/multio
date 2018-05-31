/*
 * (C) Copyright 1996-2015 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <stdlib.h>

#include "eckit/log/Log.h"
#include "eckit/config/YAMLConfiguration.h"
#include "eckit/testing/Test.h"
#include "eckit/parser/JSON.h"
#include "eckit/io/DataBlob.h"
#include "eckit/parser/JSONDataBlob.h"
#include "eckit/utils/Translator.h"

#include "multio/DataSink.h"
#include "multio/FileSink.h"
#include "multio/MultIO.h"

using namespace eckit;
using namespace eckit::testing;

namespace multio {
namespace test {

//-----------------------------------------------------------------------------

class XX {

    std::string stream_;
    std::string step_;
    std::string level_;

public: // methods

    XX(const std::string& stream) : stream_(stream), step_(), level_() {}

    void step(long step) { step_ = Translator<long,std::string>()(step); }
    void level(long level) { level_ = Translator<long,std::string>()(level); }

    void json(JSON& s) const {
        s.startObject();
        s << "stream" << stream_;
        s << "step"   << step_;
        s << "level"  << level_;
        s.endObject();
    }
};


CASE("test_multio_with_event_trigger") {

    PathName("tmp.1").unlink();
    PathName("tmp.2").unlink();

    const char* tconf =
        "{ \"triggers\" : [ "
        "{ \"type\" : \"MetadataChange\", \"host\" : \"localhost\", \"port\" : 10000, \"retries_\" : 0, \"timeout\" : 1, \"key\" : \"step\", \"values\" : [\"0\", \"3\", \"6\", \"9\", \"12\", \"24\"],"
        " \"info\" : { \"job\" : \"234\", \"app\" : \"foobar\" } },"
        "{ \"type\" : \"MetadataChange\", \"file\" : \"tmp.1\", \"key\" : \"step\", \"values\" : [\"0\", \"3\", \"6\", \"9\", \"12\", \"24\"],"
        " \"info\" : { \"job\" : \"234\", \"app\" : \"foobar\" } },"
        "{ \"type\" : \"MetadataChange\", \"file\" : \"tmp.2\", \"key\" : \"step\", \"values\" : [\"1\", \"4\", \"5\", \"6\", \"10\"],"
        " \"info\" : { \"job\" : \"234\", \"app\" : \"foobar\" } }"
                       "] }";

    ::setenv("MULTIO_CONFIG_TRIGGERS", tconf, 1);

    std::string sinks("{ \"sinks\" : [ {\"type\" : \"file\", \"path\" : \"/dev/null\"} ] }");
    eckit::YAMLConfiguration config(sinks);

    eckit::ScopedPtr<MultIO> mio(new MultIO(config));

    XX x("oper");

    for(int step = 0; step <= 24; step++) {
        for(int level = 1; level <= 5; level++) {

            Log::info() << "step " << step << " level " << level << std::endl;

            std::ostringstream os;
            JSON msg(os);

            x.step(step);
            x.level(level);
            x.json(msg);

            DataBlobPtr blob(new JSONDataBlob(os.str().c_str(), os.str().size()));

            mio->write(blob);
        }
    }

}

//-----------------------------------------------------------------------------

}  // namespace test
}  // namespace multio

int main(int argc, char** argv) {
    return run_tests(argc, argv);
}
