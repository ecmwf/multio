/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include <stdio.h>
#include <stdlib.h>

#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <unordered_set>

#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/io/FileHandle.h"
#include "eckit/io/MemoryHandle.h"
#include "eckit/log/Log.h"
#include "eckit/message/Message.h"
#include "eckit/message/Reader.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"
#include "metkit/codes/api/CodesAPI.h"
#include "metkit/mars2grib/api/Mars2Grib.h"

#include "multio/sink/DataSink.h"
#include "multio/tools/MultioTool.h"
#include "multio/tools/utils/CodesHandleToEckitMessage.h"
#include "multio/tools/utils/distGrib1ToGrib2Options.h"
#include "multio/tools/utils/grib2MarsMisc.h"
#include "multio/tools/utils/scalarGrib1ToGrib2DebugOutputs.h"

namespace multio::grib1ToGrib2 {

namespace {

void throwIfYamlModeConflicts(const eckit::option::CmdArgs& args) {
    static const char* kConflictingOptions[] = {"all",
                                                "wmo-units",
                                                "control",
                                                "exclude",
                                                "except",
                                                "filter",
                                                "packing",
                                                "model",
                                                "ncycle",
                                                "discipline-192",
                                                "on-error",
                                                "timespan-equal-to-zero",
                                                "default-ensemble-size",
                                                "convert-wave-stream-to-oper",
                                                "expver"};

    for (const auto* option : kConflictingOptions) {
        if (args.has(option)) {
            throw std::runtime_error(std::string{"--options-yaml cannot be combined with --"} + option);
        }
    }
}

void write(const metkit::codes::CodesHandle& grib, eckit::FileHandle& file) {
    eckit::Buffer buf{grib.messageSize()};
    grib.copyInto(reinterpret_cast<uint8_t*>(buf.data()), buf.size());
    file.write(buf.data(), buf.size());
}

grib2MarsMisc::FieldValueMap parseFieldValueMap(std::string s, long verbosity) {
    const std::string fieldDelim = ";";
    const std::string fieldValueDelim = "=";
    const std::string valuesDelim = ",";
    grib2MarsMisc::FieldValueMap ret;

    size_t posField = 0;
    std::string fieldAndVals;
    do {
        posField = s.find(fieldDelim);
        fieldAndVals = s.substr(0, posField);
        s.erase(0, posField + fieldDelim.length());

        size_t posFieldVal = fieldAndVals.find(fieldValueDelim);
        ASSERT(posFieldVal != std::string::npos);
        std::string field = fieldAndVals.substr(0, posFieldVal);
        fieldAndVals.erase(0, posFieldVal + fieldValueDelim.length());
        if (verbosity >= 2) {
            std::cout << "Parsed field " << field << std::endl;
        }

        size_t posVals = 0;
        std::string val;
        grib2MarsMisc::ValueSet values;
        do {
            posVals = fieldAndVals.find(valuesDelim);
            val = fieldAndVals.substr(0, posVals);
            fieldAndVals.erase(0, posVals + valuesDelim.length());

            if (verbosity >= 2) {
                std::cout << "   parsed value: " << val << std::endl;
            }

            values.insert(val);
        } while (posVals != std::string::npos);

        ret.emplace(std::move(field), std::move(values));
    } while (posField != std::string::npos);

    return ret;
}

const std::unordered_map<std::string, grib2MarsMisc::TimeSpanEqualToZeroHandling>& timeSpanEqualToZeroHandlingMap() {
    static const std::unordered_map<std::string, grib2MarsMisc::TimeSpanEqualToZeroHandling> map{
        {"log-and-ignore", grib2MarsMisc::TimeSpanEqualToZeroHandling::LogAndIgnore},
        {"ignore", grib2MarsMisc::TimeSpanEqualToZeroHandling::Ignore},
        {"copy", grib2MarsMisc::TimeSpanEqualToZeroHandling::Copy}};
    return map;
}

grib2MarsMisc::TimeSpanEqualToZeroHandling parseTimeSpanEqualToZeroHandling(const std::string& str) {
    const auto& map = timeSpanEqualToZeroHandlingMap();
    if (auto search = map.find(str); search != map.end()) {
        return search->second;
    }
    throw std::runtime_error(std::string("Unsupported --time-span-equal-to-zero value: ") + str);
}

const std::unordered_map<std::string, grib2MarsMisc::Discipline192Handling>& discipline192HandlingMap() {
    static const std::unordered_map<std::string, grib2MarsMisc::Discipline192Handling> map{
        {"log-and-ignore", grib2MarsMisc::Discipline192Handling::LogAndIgnore},
        {"ignore", grib2MarsMisc::Discipline192Handling::Ignore},
        {"try-to-handle", grib2MarsMisc::Discipline192Handling::TryToHandle},
        {"copy", grib2MarsMisc::Discipline192Handling::Copy}};
    return map;
}

grib2MarsMisc::Discipline192Handling parseDiscipline192Handling(const std::string& str) {
    const auto& map = discipline192HandlingMap();
    if (auto search = map.find(str); search != map.end()) {
        return search->second;
    }
    throw std::runtime_error(std::string("Unsupported discipline-192 handling: ") + str);
}

const std::unordered_map<std::string, grib2MarsMisc::OnErrorHandling>& onErrorHandlingMap() {
    static const std::unordered_map<std::string, grib2MarsMisc::OnErrorHandling> map{
        {"abort", grib2MarsMisc::OnErrorHandling::Abort},
        {"log-and-skip", grib2MarsMisc::OnErrorHandling::LogAndSkip},
        {"skip", grib2MarsMisc::OnErrorHandling::Skip},
        {"try-to-handle", grib2MarsMisc::OnErrorHandling::TryToHandle},
        {"copy", grib2MarsMisc::OnErrorHandling::Copy}};
    return map;
}

grib2MarsMisc::OnErrorHandling parseOnErrorHandling(const std::string& str) {
    const auto& map = onErrorHandlingMap();
    if (auto search = map.find(str); search != map.end()) {
        return search->second;
    }
    throw std::runtime_error(std::string("Unsupported --on-error value: ") + str);
}

}  // namespace

class Grib1ToGrib2V2 final : public multio::MultioTool {
public:
    Grib1ToGrib2V2(int argc, char** argv);

private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << std::endl << "Usage: " << tool << " [options] inputFile outputFile " << std::endl;
        eckit::Log::info() << std::endl
                           << "\tinputFile:\t"
                           << "GRIB file" << std::endl
                           << "\toutputFile:\t"
                           << "output file location" << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;
    void finish(const eckit::option::CmdArgs&) override;
    void execute(const eckit::option::CmdArgs& args) override;

    int numberOfPositionalArguments() const override { return 2; }
    int minimumPositionalArguments() const override { return 2; }

    long verbosity_ = 0;
    bool noOutput_ = false;
    bool useOptionsYaml_ = false;
    std::string debugOutputPrefix_;
    std::optional<eckit::LocalConfiguration> archiveProbeSinkConfig_;
    grib2MarsMisc::Grib2MarsMiscOptions grib2MarsMiscOptions_{};
};

Grib1ToGrib2V2::Grib1ToGrib2V2(int argc, char** argv) : multio::MultioTool{argc, argv} {
    options_.push_back(new eckit::option::SimpleOption<bool>("help", "Print help"));
    options_.push_back(
        new eckit::option::SimpleOption<bool>("no-output", "Does not write the file. Used for testing purposes."));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "options-yaml",
        "Load the same YAML options file used by dist-grib1-to-grib2. Cannot be combined with conversion flags."));
    options_.push_back(new eckit::option::SimpleOption<bool>(
        "all", "If specified also grib2 messages will reencoded instead of copied"));
    options_.push_back(new eckit::option::SimpleOption<bool>(
        "wmo-units", "If specified params with local units will be mapped to params with WMO units"));
    options_.push_back(new eckit::option::SimpleOption<bool>("verbose", "Sets verbosity to 2"));
    options_.push_back(new eckit::option::SimpleOption<bool>(
        "control",
        "Treat input as a control forecast: sets number=0, adjusts ensemble-related keys, and may enforce "
        "specific stream/type constraints"));
    options_.push_back(
        new eckit::option::SimpleOption<long>("verbosity",
                                              "Verbosity level: 0 (print nothing), 1 (print mars keys per message), 2 "
                                              "(print additional extraction information)"));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "exclude",
        "Keys and values to be excluded. Multiple values are separated by ','. Multiple key-values pairs are separated "
        "by ';'. Example --exclude paramId=130,131,133;levtype=pl,sfc"));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "except",
        "Keys and values to be copied verbatim (without re-encoding) when --all is active. Same syntax as --exclude. "
        "Only applies to GRIB2 messages; matching a GRIB1 message is an error. "
        "Example --except paramId=213131"));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "filter",
        "Keys and values to be included. Multiple values are separated by ','. Multiple key-values pairs are separated "
        "by ';'. Example --filter paramId=130,131,133;levtype=pl,sfc"));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "packing", "Enforce a specific packing type. Valid values are `ccsds` and `simple`."));
    options_.push_back(new eckit::option::SimpleOption<std::string>("model", "Inject the MARS key \"model\""));
    options_.push_back(
        new eckit::option::SimpleOption<long>("ncycle", "Inject the generatingProcessIdentifier (aka NCYCLE)"));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "discipline-192",
        "Options on handling fields with discipline 192 (field that are ill-formed). Values: \"log-and-ignore\" "
        "(default), \"ignore\", \"try-to-handle\""));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "on-error",
        "How to handle per-message conversion errors. Values: \"abort\" (stop on first error), \"log-and-skip\" "
        "(default, log the error and continue), \"skip\" (silently skip failing messages)"));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "timespan-equal-to-zero",
        "How to handle fields with time span equal to zero (these fields should not exist in grib2 so it is not "
        "possible to try-to-handle). Values: \"log-and-ignore\" (default), \"ignore\", \"copy\""));
    options_.push_back(new eckit::option::SimpleOption<long>(
        "default-ensemble-size",
        "Fallback value used when numberOfForecastsInEnsemble is 0 but number is non-zero. Default: 0 (throw)"));
    options_.push_back(new eckit::option::SimpleOption<bool>(
        "convert-wave-stream-to-oper", "If enabled it converts the wave stream (wave/waef) to oper stream. Default: false (throw)"));
    options_.push_back(new eckit::option::SimpleOption<std::string>(
        "expver",
        "Override expver. Default: 0 (throw)"));
}

void Grib1ToGrib2V2::init(const eckit::option::CmdArgs& args) {
    bool verbose = false;
    args.get("verbose", verbose);
    if (verbose) {
        verbosity_ = 2;
    }
    args.get("verbosity", verbosity_);
    args.get("no-output", noOutput_);

    std::string optionsYaml;
    args.get("options-yaml", optionsYaml);
    useOptionsYaml_ = !optionsYaml.empty();

    if (useOptionsYaml_) {
        throwIfYamlModeConflicts(args);
        const auto yamlOptions = distGrib1ToGrib2::loadOptionsFromYamlFile(optionsYaml);
        debugOutputPrefix_ = distGrib1ToGrib2::debugOutputPrefix(yamlOptions);
        if (yamlOptions.has("sink")) {
            archiveProbeSinkConfig_ = yamlOptions.getSubConfiguration("sink");
        }
        if (archiveProbeSinkConfig_ && debugOutputPrefix_.empty()) {
            throw std::runtime_error("YAML option sink requires debug.output-prefix in grib1-to-grib2-v2");
        }
        grib2MarsMiscOptions_ = grib2MarsMisc::makeGrib2MarsMiscOptions(yamlOptions);
        return;
    }

    bool all = false;
    args.get("all", all);
    grib2MarsMiscOptions_.copyGrib2Messages = !all;

    args.get("control", grib2MarsMiscOptions_.controlForecast);
    args.get("wmo-units", grib2MarsMiscOptions_.useWmoUnits);
    args.get("ncycle", grib2MarsMiscOptions_.ncycle);
    args.get("default-ensemble-size", grib2MarsMiscOptions_.defaultEnsembleSize);

    bool convertWaveStreamToOper = false;
    args.get("convert-wave-stream-to-oper", convertWaveStreamToOper);
    grib2MarsMiscOptions_.convertWaveStreamToOper = convertWaveStreamToOper;

    std::string packing;
    args.get("packing", packing);
    if (!packing.empty()) {
        if (packing == "ccsds" || packing == "simple") {
            grib2MarsMiscOptions_.packingOverride = packing;
        }
        else {
            throw std::runtime_error(std::string("Unsupported packing: ") + packing);
        }
    }

    std::string expver;
    args.get("expver", expver);
    if (!expver.empty()) {
        grib2MarsMiscOptions_.expverOverride = expver;
    }

    std::string model;
    args.get("model", model);
    if (!model.empty()) {
        grib2MarsMiscOptions_.modelOverride = model;
    }

    std::string excludeStr = "";
    args.get("exclude", excludeStr);
    if (!excludeStr.empty()) {
        grib2MarsMiscOptions_.exclude = parseFieldValueMap(std::move(excludeStr), verbosity_);
    }

    std::string exceptStr = "";
    args.get("except", exceptStr);
    if (!exceptStr.empty()) {
        if (grib2MarsMiscOptions_.copyGrib2Messages) {
            std::cerr << "Warning: --except has no effect without --all (GRIB2 messages are already copied verbatim)"
                      << std::endl;
        }
        grib2MarsMiscOptions_.except = parseFieldValueMap(std::move(exceptStr), verbosity_);
    }

    std::string filterStr = "";
    args.get("filter", filterStr);
    if (!filterStr.empty()) {
        grib2MarsMiscOptions_.filter = parseFieldValueMap(std::move(filterStr), verbosity_);
    }

    std::string discipline192;
    args.get("discipline-192", discipline192);
    if (!discipline192.empty()) {
        grib2MarsMiscOptions_.discipline192 = parseDiscipline192Handling(discipline192);
    }

    std::string onError;
    args.get("on-error", onError);
    if (!onError.empty()) {
        grib2MarsMiscOptions_.onError = parseOnErrorHandling(onError);
    }

    std::string timeSpanEqualToZero;
    args.get("timespan-equal-to-zero", timeSpanEqualToZero);
    if (!timeSpanEqualToZero.empty()) {
        grib2MarsMiscOptions_.timespanNonPositive = parseTimeSpanEqualToZeroHandling(timeSpanEqualToZero);
    }
}

void Grib1ToGrib2V2::finish(const eckit::option::CmdArgs&) {}

void Grib1ToGrib2V2::execute(const eckit::option::CmdArgs& args) {
    eckit::message::Reader reader{args(0)};
    std::optional<eckit::PathName> outPath;
    if (!noOutput_) {
        outPath = args(1);
    }

    if (outPath && outPath->exists()) {
        const int result = remove(((std::string)*outPath).c_str());
        if (result == 0) {
            if (verbosity_ > 0) {
                std::cout << "Removed existing file " << *outPath << std::endl;
            }
        }
        else {
            std::cerr << "Could not remove existing file " << *outPath << std::endl;
            return;
        }
    }

    std::optional<eckit::FileHandle> outputFileHandle;
    if (outPath) {
        outputFileHandle.emplace(*outPath, true);
        outputFileHandle->openForWrite(0);
    }

    ScalarDebugOutputs debugOutputs(debugOutputPrefix_);
    std::unique_ptr<sink::DataSink> archiveProbeSink;
    if (archiveProbeSinkConfig_) {
        archiveProbeSink = buildArchiveProbeSink(*archiveProbeSinkConfig_);
    }

    metkit::mars2grib::Mars2Grib encoder{};

    eckit::message::Message msg;
    std::size_t msgIndex = 0;
    std::size_t nonSuccessCount = 0;
    while ((msg = reader.next())) {
        ++msgIndex;
        try {
            std::unique_ptr<eckit::DataHandle> dh{msg.readHandle()};
            auto* mh = dynamic_cast<eckit::MemoryHandle*>(dh.get());
            ASSERT(mh != nullptr);

            auto inputHandle = metkit::codes::codesHandleFromMessageCopy(
                metkit::codes::Span<const uint8_t>(reinterpret_cast<const uint8_t*>(mh->data()), mh->size()));

            auto result = grib2MarsMisc::grib2MarsMisc(msg, grib2MarsMiscOptions_);
            const auto& extracted = result.extractedMessage;
            const auto& outcome = result.extractionOutcome;

            switch (outcome.disposition) {
                case grib2MarsMisc::MessageDisposition::Encode: {
                    if (verbosity_ > 2) {
                        std::cout << "Encoding with extracted metadata..." << std::endl;
                    }
                    std::cout << "Encoding message #" << msgIndex
                              << " to GRIB2 (total GRIB2 messages so far: " << msgIndex << ")" << std::endl;

                    decltype(encoder.encode(extracted.values, extracted.mars, extracted.misc)) preparedHandle;
                    try {
                        preparedHandle = encoder.encode(extracted.values, extracted.mars, extracted.misc);
                    }
                    catch (...) {
                        debugOutputs.writeInputMessage(ScalarDebugBucket::FailedEncode, msg);
                        throw;
                    }
                    const long isMessageValid = preparedHandle->getLong("isMessageValid");
                    if (isMessageValid != 1) {
                        std::cerr << "WARNING: Re-encoded message #" << msgIndex
                                  << " is not valid according to the GRIB2 metadata. This likely means the message is "
                                     "malformed and may fail to convert to GRIB2. ";
                    }

                    if (outputFileHandle) {
                        write(*preparedHandle, *outputFileHandle);
                    }

                    if (archiveProbeSink) {
                        try {
                            archiveProbeSink->write(tools::utils::to_eckit_message(*preparedHandle));
                            archiveProbeSink->flush();
                            debugOutputs.writeInputMessage(ScalarDebugBucket::ConvertedAndArchived, msg);
                        }
                        catch (const std::exception& e) {
                            debugOutputs.writeInputMessage(ScalarDebugBucket::FailedArchive, msg);
                            debugOutputs.writeArchiveFailureEncoded(*preparedHandle);
                            ++nonSuccessCount;
                            if (grib2MarsMiscOptions_.onError == grib2MarsMisc::OnErrorHandling::LogAndSkip) {
                                std::cerr << "Error archiving message #" << msgIndex << ": " << e.what()
                                          << " -- classified as FailedArchive" << std::endl;
                            }
                        }
                        catch (...) {
                            debugOutputs.writeInputMessage(ScalarDebugBucket::FailedArchive, msg);
                            debugOutputs.writeArchiveFailureEncoded(*preparedHandle);
                            ++nonSuccessCount;
                            if (grib2MarsMiscOptions_.onError == grib2MarsMisc::OnErrorHandling::LogAndSkip) {
                                std::cerr << "Error archiving message #" << msgIndex
                                          << ": unknown exception -- classified as FailedArchive" << std::endl;
                            }
                        }
                    }
                    else {
                        debugOutputs.writeInputMessage(ScalarDebugBucket::Converted, msg);
                    }
                    break;
                }

                case grib2MarsMisc::MessageDisposition::CopyInvalidMessage:
                    std::cerr << "WARNING: Message " << msgIndex
                              << " is not valid according to the GRIB1 metadata. This likely means the message is malformed and may fail to convert to GRIB2. Copying invalid message verbatim."
                              << std::endl;
                    [[fallthrough]];

                case grib2MarsMisc::MessageDisposition::CopyGrib2Verbatim:
                    if (outcome.disposition == grib2MarsMisc::MessageDisposition::CopyGrib2Verbatim && verbosity_ > 2) {
                        std::cout << "Copying grib2 message..." << std::endl;
                    }
                    [[fallthrough]];

                case grib2MarsMisc::MessageDisposition::CopyExceptMatched:
                    if (outcome.disposition == grib2MarsMisc::MessageDisposition::CopyExceptMatched && verbosity_ >= 1) {
                        std::cout << "except map matched — copying GRIB2 message verbatim" << std::endl;
                    }
                    [[fallthrough]];

                case grib2MarsMisc::MessageDisposition::CopyDiscipline192:
                    if (outcome.disposition == grib2MarsMisc::MessageDisposition::CopyDiscipline192) {
                        std::cout << "Copying message with discipline 192 (paramId: " << inputHandle->getLong("paramId")
                                  << ")" << std::endl;
                    }
                    [[fallthrough]];

                case grib2MarsMisc::MessageDisposition::CopyTimespanNonPositive:
                    if (outcome.disposition == grib2MarsMisc::MessageDisposition::CopyTimespanNonPositive && verbosity_ > 0) {
                        std::cerr << "WARNING: Copying message with non-positive timespan (paramId: "
                                  << inputHandle->getLong("paramId") << ")" << std::endl;
                    }
                    if (outputFileHandle) {
                        write(*inputHandle, *outputFileHandle);
                    }
                    debugOutputs.writeInputMessage(bucketForOutcome(outcome.code), msg);
                    break;

                case grib2MarsMisc::MessageDisposition::SkipExcluded:
                    if (verbosity_ >= 2) {
                        std::cout << "exclude map matched... skipping message" << std::endl;
                    }
                    debugOutputs.writeInputMessage(bucketForOutcome(outcome.code), msg);
                    break;

                case grib2MarsMisc::MessageDisposition::SkipFilteredOut:
                    if (verbosity_ >= 2) {
                        std::cout << "filter map did not match... skipping message" << std::endl;
                    }
                    debugOutputs.writeInputMessage(bucketForOutcome(outcome.code), msg);
                    break;

                case grib2MarsMisc::MessageDisposition::SkipInvalidMessage:
                    if (grib2MarsMiscOptions_.onError == grib2MarsMisc::OnErrorHandling::LogAndSkip) {
                        std::cerr << "Error: Message " << msgIndex
                                  << " is not valid according to the GRIB1 metadata. This likely means the message is malformed and may fail to convert to GRIB2. Skipping message."
                                  << std::endl;
                    }
                    ++nonSuccessCount;
                    debugOutputs.writeInputMessage(bucketForOutcome(outcome.code), msg);
                    break;

                case grib2MarsMisc::MessageDisposition::SkipDiscipline192:
                    if (grib2MarsMiscOptions_.discipline192 == grib2MarsMisc::Discipline192Handling::LogAndIgnore) {
                        std::cout << "Excluding message with discipline 192 (paramId: " << inputHandle->getLong("paramId")
                                  << ")" << std::endl;
                    }
                    debugOutputs.writeInputMessage(bucketForOutcome(outcome.code), msg);
                    break;

                case grib2MarsMisc::MessageDisposition::SkipTimespanNonPositive:
                    if (grib2MarsMiscOptions_.timespanNonPositive == grib2MarsMisc::TimeSpanEqualToZeroHandling::LogAndIgnore) {
                        std::cerr << "WARNING: Skipping message with non-positive timespan (paramId: "
                                  << inputHandle->getLong("paramId") << ")" << std::endl;
                    }
                    else if (grib2MarsMiscOptions_.timespanNonPositive == grib2MarsMisc::TimeSpanEqualToZeroHandling::Ignore
                             && verbosity_ > 0) {
                        std::cerr << "WARNING:Ignoring message with non-positive timespan (paramId: "
                                  << inputHandle->getLong("paramId") << ")" << std::endl;
                    }
                    debugOutputs.writeInputMessage(bucketForOutcome(outcome.code), msg);
                    break;

                case grib2MarsMisc::MessageDisposition::FailToExtract:
                    if (grib2MarsMiscOptions_.onError == grib2MarsMisc::OnErrorHandling::Abort) {
                        throw std::runtime_error(outcome.detail.empty() ? outcome.reason : outcome.detail);
                    }
                    ++nonSuccessCount;
                    if (grib2MarsMiscOptions_.onError == grib2MarsMisc::OnErrorHandling::LogAndSkip) {
                        std::cerr << "Error converting message #" << msgIndex << ": "
                                  << (outcome.detail.empty() ? outcome.reason : outcome.detail) << " -- skipping"
                                  << std::endl;
                    }
                    debugOutputs.writeInputMessage(bucketForOutcome(outcome.code), msg);
                    break;

                case grib2MarsMisc::MessageDisposition::FailToEncode:
                    debugOutputs.writeInputMessage(ScalarDebugBucket::FailedEncode, msg);
                    throw std::runtime_error(outcome.detail.empty() ? outcome.reason : outcome.detail);

                case grib2MarsMisc::MessageDisposition::FailToArchive:
                    debugOutputs.writeInputMessage(ScalarDebugBucket::FailedArchive, msg);
                    throw std::runtime_error(outcome.detail.empty() ? outcome.reason : outcome.detail);
            }
        }
        catch (const std::exception& e) {
            if (grib2MarsMiscOptions_.onError == grib2MarsMisc::OnErrorHandling::Abort) {
                throw;
            }
            ++nonSuccessCount;
            if (grib2MarsMiscOptions_.onError == grib2MarsMisc::OnErrorHandling::LogAndSkip) {
                std::cerr << "Error converting message #" << msgIndex << ": " << e.what() << " -- skipping"
                          << std::endl;
            }
            continue;
        }
    }

    if (nonSuccessCount > 0) {
        std::cerr << "grib1-to-grib2: observed " << nonSuccessCount
                  << " non-success message(s) during conversion/archive probing" << std::endl;
    }

    if (debugOutputs.enabled()) {
        std::cerr << "grib1-to-grib2: debug bucket counts " << debugOutputs.summary() << std::endl;
    }

    if (outputFileHandle) {
        outputFileHandle->close();
    }
}

}  // namespace multio::grib1ToGrib2

int main(int argc, char** argv) {
    multio::grib1ToGrib2::Grib1ToGrib2V2 tool(argc, argv);
    return tool.start();
}
