/*
 * (C) Copyright 1996-2015 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <sstream>

#include "eckit/runtime/Tool.h"
#include "eckit/io/Buffer.h"
#include "eckit/config/JSONConfiguration.h"

#include "multio/MultIO.h"

using namespace eckit;

namespace multio {

//----------------------------------------------------------------------------------------------------------------------

class Mx : public Tool {
public:

    Mx(int argc,char **argv): Tool(argc,argv) {}

    ~Mx() {}

    virtual void run();
};

void Mx::run()
{
    Buffer buffer(1024*1024);

    /// @TODO load some data

    std::stringstream oss;
    oss << "{ \"sinks\" : ["
            "    {"
            "        \"type\" : \"file\","
            "        \"path\" : \"foo.grib\","
            "        \"truncate\": true"
            "    },"
            "    {"
            "        \"type\" : \"file\","
            "        \"path\" : \"bar.grib\""
            "    },"
            "    {"
            "        \"type\" : \"file\","
            "        \"path\" : \"/dev/null\""
            "    }"
            "   ] }" << std::endl;

    JSONConfiguration config(oss);

    MultIO msink(config);

    msink.open();

    msink.write(buffer, buffer.size());

    msink.close();
}

} // namespace multio

//----------------------------------------------------------------------------------------------------------------------

using namespace multio;

int main(int argc,char **argv)
{
    Mx app(argc,argv);
    app.start();
    return 0;
}
