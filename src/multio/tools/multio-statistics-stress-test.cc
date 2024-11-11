/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany
/// @author Simon Smart
/// @author Tiago Quintino

/// @date Oct 2019

#include <fstream>
#include <regex>
#include <cstdlib>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/io/FileHandle.h"
#include "eckit/io/PeekHandle.h"
#include "eckit/io/StdFile.h"
#include "eckit/log/Log.h"
#include "eckit/maths/Functions.h"
#include "eckit/message/Decoder.h"
#include "eckit/message/Message.h"
#include "eckit/message/Reader.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"
#include "eckit/value/Value.h"
#include "metkit/codes/CodesSplitter.h"
#include "multio/ifsio/ifsio.h"
#include "multio/tools/MultioTool.h"

namespace multio {
namespace test {

namespace {

class MetadataSetter : public eckit::LocalConfiguration {
public:
    using eckit::LocalConfiguration::getDouble;
    using eckit::LocalConfiguration::getLong;
    using eckit::LocalConfiguration::getString;
    using eckit::LocalConfiguration::has;

    template <typename T>
    void setValue(const std::string& key, const T& value) {
        set(key, value);
    }

    template <typename T>
    T get(const std::string& key) {
        T value;
        eckit::LocalConfiguration::get(key, value);
        return value;
    }

    std::vector<std::string> keys() { return eckit::LocalConfiguration::keys(); }
};
}  // namespace


class MultioStatisticsStressTester final : public multio::MultioTool {
public:  // methods
    MultioStatisticsStressTester(int argc, char** argv);

private:

    void usage(const std::string& tool) const override {
        eckit::Log::info() << std::endl << "Usage: " << tool << " inputFile [options]" << std::endl;
        eckit::Log::info() << std::endl
                           << "\tinputFile:\t"
                           << "GRIB or BUFR file" << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

    void finish(const eckit::option::CmdArgs&) override;

    void execute(const eckit::option::CmdArgs& args) override;

    int numberOfPositionalArguments() const override { return 0; }
    int minimumPositionalArguments() const override { return 0; }

    long nFlushFreq_;
    long nSteps_;
    long nParamID_;
    long nLevels_;
    long nValues_;
    long precisionRatio_;
    std::string configPath_ = "";

};

MultioStatisticsStressTester::MultioStatisticsStressTester(int argc, char** argv) :multio::MultioTool{argc, argv} ,nFlushFreq_{1},nSteps_{5},nParamID_{1},nLevels_{1}, nValues_{10000},precisionRatio_{100}
    {

    options_.push_back(
        new eckit::option::SimpleOption<long>("nFlushFreq",
                                              "number of steps between flush "));

    options_.push_back(
        new eckit::option::SimpleOption<long>("nSteps",
                                              "number of Steps to be generated "));

    options_.push_back(
        new eckit::option::SimpleOption<long>("nParams",
                                              "number of param ID to be generated "));
    options_.push_back(
        new eckit::option::SimpleOption<long>("nLevels",
                                              "number of levels to be generated"));

    options_.push_back(
        new eckit::option::SimpleOption<long>("nValues",
                                              "number of values per message"));

    options_.push_back(
        new eckit::option::SimpleOption<long>("precisionRatio",
                                              "percentage of messages in double wrt single precision (80 means that 80% of messages will be in double precision)"));

    options_.push_back(
        new eckit::option::SimpleOption<std::string>("plans", "Path to YAML/JSON file containing plans and actions."));
}

void MultioStatisticsStressTester::init(const eckit::option::CmdArgs& args) {

    args.get("nFlushFreq",     nFlushFreq_ );
    args.get("nSteps",         nSteps_ );
    args.get("nParams",        nParamID_ );
    args.get("nLevels",        nLevels_ );
    args.get("nValues",        nValues_ );
    args.get("precisionRatio", precisionRatio_ );

    args.get("plans", configPath_);

    if (!configPath_.empty()) {
        ::setenv("MULTIO_PLANS_FILE", configPath_.c_str(), 1);
    }
}

void MultioStatisticsStressTester::finish(const eckit::option::CmdArgs&) {}

void MultioStatisticsStressTester::execute(const eckit::option::CmdArgs& args) {
    using eckit::message::ValueRepresentation;

    MetadataSetter metadata;

    eckit::Buffer data_d;
    eckit::Buffer data_f;
    data_d.resize( nValues_*sizeof(double) );
    data_f.resize( nValues_*sizeof(float) );
    double* vald = static_cast<double*>(data_d.data());
    float* valf = static_cast<float*>(data_f.data());
    auto rndd = [](){ return (double)(rand()) / (double)(rand()); };
    auto rndf = [](){ return (float)(rand()) / (float)(rand()); };
    for ( int d=0; d<nValues_; ++d  ){
        vald[d] = rndd();
        valf[d] = vald[d];
    }

    for ( int i=0; i<nSteps_; i++ ){

            // Generic metadata (not enough for encoding)
            metadata.set("gribEdition","2" );

            metadata.set("domain","g" );
            metadata.set("expver","hvi1" );
            metadata.set("class","rd" );
            metadata.set("type","fc" );
            metadata.set("stream","lwda" );
            metadata.set("anoffset",9 );



            metadata.set("levtype","ml" );
            metadata.set("gridType","reduced_gg" );
            metadata.set("name","Logarithm of surface pressure" );
            metadata.set("shortName","lnsp" );

            // Time management
            metadata.set("date",long(20200120) );
            metadata.set("time",long(0) );
            metadata.set("timeStep",long(3600) );
            metadata.set("step-frequency", long(1) );


            // Set the step idx
            metadata.set("step",i );
            metadata.set("stepId",i );

            long msgID = 0;
            // Push 
            for ( long j=0; j< nParamID_; j++ ){
                metadata.set("paramId", j*10);
                metadata.set("param", j*10 );
                for ( long k=0; k< nLevels_; k++ ){
                    msgID++;
                    metadata.set("level", k*100);
                    metadata.set("levelist", k*100);

                    long ratio = static_cast<long>(100*(static_cast<double>(msgID) / static_cast<double>(nParamID_*nLevels_)));
                    if ( ratio <= precisionRatio_  ){
                        metadata.set("precision", "double");
                        metadata.set("globalSize", data_d.size() / sizeof(double));
                        size_t words = eckit::round(data_d.size(), sizeof(fortint)) / sizeof(fortint);
                        fortint iwords = static_cast<fortint>(words);

                        if (imultio_write_raw_(&metadata, reinterpret_cast<const void*>(data_d.data()), &iwords)) {
                            ASSERT(false);
                        }                        
                    }
                    else {
                        metadata.set("precision", "single");
                        metadata.set("globalSize", data_f.size() / sizeof(float));
                        size_t words = eckit::round(data_f.size(), sizeof(fortint)) / sizeof(fortint);
                        fortint iwords = static_cast<fortint>(words);

                        if (imultio_write_raw_(&metadata, reinterpret_cast<const void*>(data_f.data()), &iwords)) {
                            ASSERT(false);
                        }
                    }

                }
            }


            if ( (i+1)%nFlushFreq_ == 0 || nFlushFreq_==1 ){
                if (imultio_flush_()) {
                    ASSERT(false);
                }
            }

    }
}

}  // namespace test
}  // namespace multio

//---------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    multio::test::MultioStatisticsStressTester tool(argc, argv);
    return tool.start();
}
