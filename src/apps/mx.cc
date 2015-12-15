/*
 * (C) Copyright 1996-2015 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "eckit/runtime/Tool.h"
#include "eckit/io/Buffer.h"
#include "eckit/config/LocalConfiguration.h"

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
    std::string key("myfdb");
    Buffer buffer(1024*1024);

    /// @TODO load some data

    LocalConfiguration config;

    /// @TODO populate config

    MultIO msink(config);

    msink.open(key);

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
