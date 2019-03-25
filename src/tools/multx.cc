/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <sstream>
#include <unistd.h>

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/config/Resource.h"
#include "eckit/io/Buffer.h"
#include "eckit/io/DataBlob.h"
#include "eckit/io/Offset.h"
#include "eckit/log/Log.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"
#include "eckit/runtime/Tool.h"

#include "metkit/grib/MetFile.h"
#include "metkit/grib/GribDataBlob.h"

#include "multio/MultIO.h"

using namespace eckit;
using namespace multio;

//using namespace eckit::option;

//----------------------------------------------------------------------------------------------------------------------

class Multx : public Tool {

public:

    Multx(int argc,char **argv) :
        Tool(argc,argv) {

        defaultConfig_ << "{ \"journalfile\": \"currentjournal\","
              " \"journaled\": true,"
              " \"sinks\" : ["
              "    {"
              "        \"journalAlways\": true,"
              "        \"type\" : \"file\","
              "        \"path\" : \"foo.grib\""
              "    },"
              // "    {"
              // "        \"type\" : \"fdb4\""
              // "    },"
              //"    {"
              //"        \"type\" : \"hermes\","
              //"        \"failOnError\" : false"
              //"    },"
              "    {"
              "        \"type\" : \"file\","
              "        \"path\" : \"bar.grib\""
              //"    },"
              //"    {"
              //"        \"journalAlways\": true,"
              //"        \"type\" : \"file\","
              //"        \"path\" : \"/dev/null\""
              "    }"
              "   ] }";
    }

private: // methods

    virtual void run();

    static void usage(const std::string &tool);

private: // members

    std::stringstream defaultConfig_;
};

//----------------------------------------------------------------------------------------------------------------------

void Multx::usage(const std::string &tool) {

    eckit::Log::info()
            << std::endl << "Usage: " << tool << " [--config=<yaml-path>] <grib>" << std::endl
            << std::endl << "Examples: " << std::endl
            << "  % " << tool << " input.grib" << std::endl
            << "  % " << tool << " --config=sinks.yaml input.grib" << std::endl;
}

void Multx::run()
{
    // Read the input

    std::vector<option::Option*> options;
    options.push_back(new option::SimpleOption<PathName>("config", "The YAML configuration to use"));
    option::CmdArgs args(&Multx::usage, options, 1, 1);

    // Which config should we use?

    std::unique_ptr<YAMLConfiguration> config;
    if (args.has("config")) {
        std::string configPath;
        args.get("config", configPath);
        config.reset(new YAMLConfiguration(configPath));
    } else {
        config.reset(new YAMLConfiguration(defaultConfig_));
    }

    //    std::vector<const Option *> options;
    //    options.push_back(new SimpleOption<eckit::PathName>("in", "Path to input file"));
    //    Args args(&usage, 0, options);
    Log::info() << "Initialising MultIO data sink" << std::endl;

    MultIO msink(*config);

    ASSERT(args.count() == 1);
    PathName path = args(0);

    Log::info() << "Opening GRIB file : " << path << std::endl;

    static long gribBufferSize = eckit::Resource<long>("gribBufferSize", 64*1024*1024);

    Buffer buffer(gribBufferSize);

    long len = 0;

    metkit::grib::MetFile file( path );

    Log::status() << "Analysing GRIB data" << std::endl;

    size_t nMsg = 0;

    while( (len = file.read(buffer)) != 0 )
    {
        DataBlobPtr blob = std::make_shared<metkit::grib::GribDataBlob>(buffer, len);

        ASSERT((size_t)len < buffer.size());

        const char *p = buffer + len - 4;

        if(p[0] != '7' || p[1] != '7' || p[2] != '7' || p[3] != '7')
            throw eckit::SeriousBug("No 7777 found");

        ++nMsg;

        msink.write(blob);

//        msink.write(h.message(), h.length(), jr, h.metadata());
    }

    msink.flush();

    while( ! msink.ready() )
    {
        ::sleep(1);
    }

    msink.commitJournal();

    msink.report(Log::info());
}

//----------------------------------------------------------------------------------------------------------------------

using namespace multio;

int main(int argc,char **argv)
{
    Multx app(argc,argv);
    return app.start();
}
