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
#include "multio/message/Glossary.h"

using multio::message::glossary;

namespace multio {
namespace test {

namespace {
class TempFile {
    const std::string path_;

public:
    TempFile(std::string&& path) : path_{std::move(path)} {}
    ~TempFile() { std::remove(path_.c_str()); }

    const std::string& path() const { return path_; }
};

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

void parseStepRange(std::string const& stepRange, long& startStep, long& endStep) {
    static const std::regex period_grammar("([0-9]+)-([0-9]+)");
    std::smatch match;
    if (std::regex_match(stepRange, match, period_grammar)) {
        startStep = std::stol(match[1].str());
        endStep = std::stol(match[2].str());
    }
    else {
        throw eckit::SeriousBug("Wrong grammar in period definition : " + stepRange, Here());
    }
};

}  // namespace

class MultioFeed final : public multio::MultioTool {
public:  // methods
    MultioFeed(int argc, char** argv);

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

    bool subtocExists() const;

    int numberOfPositionalArguments() const override { return 1; }
    int minimumPositionalArguments() const override { return 1; }


    eckit::PathName fdbRootPath_;

    bool testSubtoc_ = false;
    bool decodeDoubleData_ = false;
    bool decodeSingleData_ = false;
    std::string configPath_ = "";
    std::string stepRange_ = "";
    long minStep_ = -1;
    long maxStep_ = 1000000000;
};

MultioFeed::MultioFeed(int argc, char** argv) :
    multio::MultioTool{argc, argv}, fdbRootPath_{"~multio/multio/tests/fdb/root"} {
    options_.push_back(new eckit::option::SimpleOption<bool>("test-subtoc", "Test if subtoc has been created"));
    options_.push_back(
        new eckit::option::SimpleOption<bool>("decode",
                                              "Decode messages and pass raw data with metadata through the pipeline "
                                              "(with data in double precision)"));
    options_.push_back(
        new eckit::option::SimpleOption<bool>("decodeSingle",
                                              "Decode messages and pass raw data with metadata through the pipeline "
                                              "(with data in single precision)"));
    options_.push_back(
        new eckit::option::SimpleOption<std::string>("plans", "Path to YAML/JSON file containing plans and actions."));

    options_.push_back(
        new eckit::option::SimpleOption<std::string>("stepRange", "Range of steps to process (e.g. 0-23)"));
}

void MultioFeed::init(const eckit::option::CmdArgs& args) {
    args.get("test-subtoc", testSubtoc_);
    if (testSubtoc_) {
        std::system(std::string{"rm -rf " + fdbRootPath_.asString() + "/*"}.c_str());
        fdbRootPath_.mkdir();
    }
    args.get("decode", decodeDoubleData_);
    args.get("decodeSingle", decodeSingleData_);

    if (decodeDoubleData_ && decodeSingleData_) {
        throw eckit::UserError{"Both double and single precision requested", Here()};
    }

    args.get("plans", configPath_);

    if (args.has("stepRange")) {
        args.get("stepRange", stepRange_);
    }
    else {
        stepRange_ = "0-1000000000";
    }

    parseStepRange(stepRange_, minStep_, maxStep_);

    if (!configPath_.empty()) {
        ::setenv("MULTIO_PLANS_FILE", configPath_.c_str(), 1);
    }
}

void MultioFeed::finish(const eckit::option::CmdArgs&) {}

void MultioFeed::execute(const eckit::option::CmdArgs& args) {
    using eckit::message::ValueRepresentation;
    eckit::message::Reader reader{args(0)};

    eckit::message::Message msg;

    while ((msg = reader.next())) {
        if (decodeDoubleData_ || decodeSingleData_) {
            MetadataSetter metadata;
            eckit::message::TypedSetter<MetadataSetter> gatherer{metadata};
            eckit::message::GetMetadataOptions mdOpts{};
            mdOpts.valueRepresentation = ValueRepresentation::Native;
            mdOpts.nameSpace = "mars";
            msg.getMetadata(gatherer, mdOpts);

            MetadataSetter metadataDetailed;
            eckit::message::TypedSetter<MetadataSetter> gathererDetailed{metadataDetailed};
            mdOpts.valueRepresentation = ValueRepresentation::Native;
            mdOpts.nameSpace = "";
            msg.getMetadata(gathererDetailed, mdOpts);

            if (metadataDetailed.has("stepRange")) {
                long step = metadataDetailed.getLong("stepRange");
                if (step < minStep_ || step > maxStep_) {
                    continue;
                }
            }

            if (metadataDetailed.has("step")) {
                long step = metadataDetailed.getLong("step");
                if (step < minStep_ || step > maxStep_) {
                    continue;
                }
                // metadata.set("step", metadataDetailed.getLong("step"));
            }

            if (metadataDetailed.has("startStep") && metadataDetailed.has("endStep")) {
                long startStep = metadataDetailed.getLong("startStep");
                long endStep = metadataDetailed.getLong("endStep");
                if (startStep < minStep_ || endStep > maxStep_) {
                    continue;
                }
                // metadata.set("startStep", metadataDetailed.getLong("startStep"));
                // metadata.set("endStep", metadataDetailed.getLong("endStep"));
            }

            if (!metadataDetailed.has("startStep") && metadataDetailed.has("endStep")) {
                long endStep = metadataDetailed.getLong("endStep");
                if (endStep < minStep_ || endStep > maxStep_) {
                    continue;
                }
                // metadata.set("endStep", metadataDetailed.getLong("endStep"));
            }

            // Step gets extracted as string instead of long (because it could be ar ange).... we don't like step
            // anayway... startStep/endStep is prefered
            if (metadata.has(glossary().step))
                metadata.set(glossary().step, metadata.getLong("step"));

            if (metadataDetailed.has("gridType"))
                metadata.set(glossary().gridType, metadataDetailed.getString("gridType"));

            if (metadataDetailed.has("startStep"))
                metadata.set(glossary().startStep, metadataDetailed.getLong("startStep"));
            if (metadataDetailed.has("endStep"))
                metadata.set(glossary().endStep, metadataDetailed.getLong("endStep"));

            // Maybe use gridType?
            if (metadataDetailed.getBool("sphericalHarmonics", false)) {
                metadata.set(glossary().sphericalHarmonics, true);

                if (metadataDetailed.has("complexPacking"))
                    metadata.set(glossary().complexPacking, metadataDetailed.getLong("complexPacking"));
                if (metadataDetailed.has("generatingProcessIdentifier"))
                    metadata.set(glossary().generatingProcessIdentifier,
                                 metadataDetailed.getLong("generatingProcessIdentifier"));
                if (metadataDetailed.has("J"))
                    metadata.set(glossary().pentagonalResolutionParameterJ, metadataDetailed.getLong("J"));
                if (metadataDetailed.has("K"))
                    metadata.set(glossary().pentagonalResolutionParameterK, metadataDetailed.getLong("K"));
                if (metadataDetailed.has("M"))
                    metadata.set(glossary().pentagonalResolutionParameterM, metadataDetailed.getLong("M"));
                if (metadataDetailed.has("JS"))
                    metadata.set(glossary().subSetJ, metadataDetailed.getLong("JS"));
                if (metadataDetailed.has("KS"))
                    metadata.set(glossary().subSetK, metadataDetailed.getLong("KS"));
                if (metadataDetailed.has("MS"))
                    metadata.set(glossary().subSetM, metadataDetailed.getLong("MS"));

                // Seems not to be settable in codes
                metadata.set(glossary().unpackedSubsetPrecision, 1);
            }

            // Name is not required but convenient to print...
            if (metadataDetailed.has("name")) {
                metadata.set(glossary().name, metadataDetailed.getString("name"));
            }
            if (metadataDetailed.has("shortName")) {
                metadata.set(glossary().shortName, metadataDetailed.getString("shortName"));
            }
            if (metadataDetailed.has("paramId")) {
                metadata.set(glossary().paramId, metadataDetailed.getLong("paramId"));
            }
            if (metadataDetailed.has("param")) {
                metadata.set(glossary().param, metadataDetailed.getString("param"));
                if (!metadata.has(glossary().paramId)) {
                    metadata.set(glossary().paramId, metadataDetailed.getLong("param"));
                }
            }
            if (metadataDetailed.has("GRIBEditionNumber")) {
                metadata.set(glossary().gribEdition, metadataDetailed.getString("GRIBEditionNumber"));
            }
            if (!metadata.has(glossary().level) && metadataDetailed.has("level")) {
                metadata.set(glossary().level, metadataDetailed.getLong("level"));
            }

            // Inject metadata needed for statistics
            if (!metadata.has(glossary().timeStep)) {
                metadata.set(glossary().timeStep, 3600);
            }
            if (!metadata.has(glossary().stepFrequency)) {
                metadata.set(glossary().stepFrequency, 1);
            }

            // Metadata required to handle missing values in statistics and interpolation
            if (metadataDetailed.has("bitmapPresent")) {
                metadata.set(glossary().bitmapPresent, metadataDetailed.getBool("bitmapPresent"));
            }
            if (metadataDetailed.has("missingValue")) {
                metadata.set(glossary().missingValue, metadataDetailed.getDouble("missingValue"));
            }

            // Multio pipelines require hhmmss format
            if (metadata.has(glossary().time)) {
                auto time = metadata.getLong("time");
                metadata.set(glossary().time, time);
            }

            eckit::Buffer data = msg.decode();

            metadata.set(glossary().globalSize, data.size() / sizeof(double));

            if (decodeDoubleData_) {
                metadata.set(glossary().precision, "double");
                size_t words = eckit::round(data.size(), sizeof(fortint)) / sizeof(fortint);
                fortint iwords = static_cast<fortint>(words);

                if (imultio_write_raw_(&metadata, reinterpret_cast<const void*>(data.data()), &iwords)) {
                    ASSERT(false);
                }
            }
            else {
                metadata.set(glossary().precision, "single");
                size_t words
                    = eckit::round(data.size() / sizeof(double) * sizeof(float), sizeof(fortint)) / sizeof(fortint);
                fortint iwords = static_cast<fortint>(words);

                std::vector<float> tmp(data.size() / sizeof(double), 0.0);
                const double* srcData = reinterpret_cast<const double*>(data.data());
                for (int i = 0; i < tmp.size(); ++i) {
                    tmp[i] = float(srcData[i]);
                }
                if (imultio_write_raw_(&metadata, reinterpret_cast<const void*>(tmp.data()), &iwords)) {
                    ASSERT(false);
                }
            }
        }
        else {
            size_t words = eckit::round(msg.length(), sizeof(fortint)) / sizeof(fortint);

            fortint iwords = static_cast<fortint>(words);

            if (imultio_write_(msg.data(), &iwords)) {
                ASSERT(false);
            }
        }

        if (imultio_flush_()) {
            ASSERT(false);
        }
    }

    if (imultio_flush_last_()) {
        ASSERT(false);
    }

    if (testSubtoc_) {
        ASSERT(subtocExists());
    }
}

bool MultioFeed::subtocExists() const {
    TempFile file{"tmp.out"};

    std::string cmd{"find " + fdbRootPath_.asString() + " -name toc* > " + file.path()};
    std::system(cmd.c_str());

    const std::regex subtoc{"^toc\\.[0-9]{8}\\.[0-9]{6}\\..*", std::regex_constants::egrep};

    std::ifstream ifstrm{file.path().c_str()};
    std::string line;
    while (std::getline(ifstrm, line)) {
        auto fname = line.substr(line.rfind("/") + 1);
        if (std::regex_match(fname, subtoc)) {
            return true;
        }
    }
    return false;
}

}  // namespace test
}  // namespace multio

//---------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    multio::test::MultioFeed tool(argc, argv);
    return tool.start();
}
