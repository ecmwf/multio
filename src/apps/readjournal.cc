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
#include "eckit/log/Log.h"

#include "multio/MultIO.h"

using namespace eckit;

namespace multio {

//--------------------------------------------------------------------------------------------------

class ReadJournal : public Tool {
public:

    ReadJournal(int argc,char **argv): Tool(argc,argv) {}

    ~ReadJournal() {}

    virtual void run();
};

void ReadJournal::run()
{
    eckit::Log::info() << "We are reading the journal ... (not)\n";
        
    eckit::Log::info() << std::flush;
}

} // namespace multio

//--------------------------------------------------------------------------------------------------

using namespace multio;

int main(int argc,char **argv)
{
    ReadJournal app(argc,argv);
    app.start();
    return 0;
}

