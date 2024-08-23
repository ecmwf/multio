
#include "MultioClient.h"

#include <algorithm>
#include <fstream>
#include <iomanip>
#include <unordered_set>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/log/Statistics.h"
#include "eckit/types/DateTime.h"

#include "multio/LibMultio.h"
#include "multio/message/Message.h"
#include "multio/transport/TransportRegistry.h"
#include "multio/util/logfile_name.h"

using multio::message::Message;
using multio::message::Peer;

#ifdef MULTIO_CLIENT_MEMORY_PROFILE_ENABLED

#include "multio/util/MemoryInformation.h"
#include "multio/util/TraceEventIds.h"
#include "multio/util/Tracer.h"

using namespace multio::util;

namespace {
const auto tracerMemoryReportPeriod = std::chrono::seconds(1);
const auto tracerFlushPeriod = std::chrono::minutes(10);

const auto tracerNumberOfChunks = 8;
const auto tracerEventsPerChunk = 32768;

const auto tracerValueMask = 0xFFFFFFFFULL;
const auto unitShiftAmount = 32;

const std::unordered_map<multio::util::InformationTypes, uint64_t> infoTypeToTraceIdMapping = {
    { multio::util::InformationTypes::PeakVirtualMemory, MULTIO_PEAK_VIRTUAL_MEMORY },
    { multio::util::InformationTypes::VirtualMemory, MULTIO_VIRTUAL_MEMORY },
    { multio::util::InformationTypes::LockedVirtualMemory, MULTIO_LOCKED_VIRTUAL_MEMORY },
    { multio::util::InformationTypes::PinnedVirtualMemory, MULTIO_PINNED_VIRTUAL_MEMORY },
    { multio::util::InformationTypes::MaximumResidentMemory, MULTIO_MAXIMUM_RESIDENT_MEMORY },
    { multio::util::InformationTypes::ResidentMemory, MULTIO_RESIDENT_MEMORY },
    { multio::util::InformationTypes::AnonimousResidentMemory, MULTIO_ANONIMOUS_RESIDENT_MEMORY },
    { multio::util::InformationTypes::FileMappingResidentMemory, MULTIO_FILE_MAPPING_RESIDENT_MEMORY },
    { multio::util::InformationTypes::SharedResidentMemory, MULTIO_SHARED_RESIDENT_MEMORY },
    { multio::util::InformationTypes::DataVirtualMemory, MULTIO_DATA_VIRTUAL_MEMORY },
    { multio::util::InformationTypes::StackVirtualMemory, MULTIO_STACK_VIRTUAL_MEMORY },
    { multio::util::InformationTypes::TextSegmentVirtualMemory, MULTIO_TEXT_SEGMENT_VIRTUAL_MEMORY },
    { multio::util::InformationTypes::SharedLibraryTextVirtualMemory, MULTIO_SHARED_LIBRARY_VIRTUAL_MEMORY },
    { multio::util::InformationTypes::PageTableEntryVirtualMemory, MULTIO_PAGE_TABLE_ENTRY_VIRTUAL_MEMORY },
    { multio::util::InformationTypes::SecondLevelPageTableEntryVirtualMemory, MULTIO_SECOND_LEVEL_PAGE_TABLE_ENTRY_VIRTUAL_MEMORY },
    { multio::util::InformationTypes::SwappedOutVirtualMemory, MULTIO_SWAPPED_OUT_VIRTUAL_MEMORY },
    { multio::util::InformationTypes::HugeTablesMemory, MULTIO_HUGE_TABLE_MEMORY },
};

const std::unordered_map<multio::util::InformationSizeUnits, uint64_t> sizeUnitToTraceValueMapping = {
    { multio::util::InformationSizeUnits::Bytes, 0},
    { multio::util::InformationSizeUnits::KiloBytes, 1},
    { multio::util::InformationSizeUnits::MegaBytes, 2},
    { multio::util::InformationSizeUnits::GigaBytes, 3},
};

multio::util::Tracer tracer(tracerNumberOfChunks, tracerEventsPerChunk, "./multio_memory.bin");

void reportMemoryUsage() {
    const multio::util::MemoryInformation usage;
    const auto keys = usage.getAvailableKeys();

    for (const auto key : keys) {
        const auto item = usage.get(key);

        const auto id = infoTypeToTraceIdMapping.at(key);
        const auto value = item.Value & tracerValueMask;
        const uint64_t unit = sizeUnitToTraceValueMapping.at(item.Unit) << unitShiftAmount;

        tracer.recordEvent(id | unit | value);
    }
}

auto last_report_time = std::chrono::system_clock::now();
auto last_flush_time = std::chrono::system_clock::now();

}

#endif

namespace multio::server {

using config::ComponentConfiguration;

namespace {

eckit::LocalConfiguration getClientConf(const MultioConfiguration& multioConf) {
    if (multioConf.parsedConfig().has("client")) {
        return multioConf.parsedConfig().getSubConfiguration("client");
    }

    // Make client work when using only action pipelines
    if (multioConf.parsedConfig().has("plans")) {
        return multioConf.parsedConfig();
    }

    std::ostringstream oss;
    oss << "Configuration 'client' not found in configuration file " << multioConf.configFile();
    throw eckit::UserError(oss.str());
}

}  // namespace

MultioClient::MultioClient(const eckit::LocalConfiguration& conf, MultioConfiguration&& multioConf) :
    MultioConfigurationHolder(std::move(multioConf), config::LocalPeerTag::Client),
    FailureAware(ComponentConfiguration(conf, multioConfig())) {
    totClientTimer_.start();

    std::ofstream logFile{util::logfile_name(), std::ios_base::app};

    struct ::timeval tstamp;
    ::gettimeofday(&tstamp, 0);
    auto mSecs = tstamp.tv_usec;

    logFile << "MultioClient starts at " << eckit::DateTime{static_cast<double>(tstamp.tv_sec)}.time().now() << ":"
            << std::setw(6) << std::setfill('0') << mSecs << " -- ";


    // TODO: Put the whole plan list in a separate class and make this logic reusable
    std::unordered_set<std::string> planNames;
    LOG_DEBUG_LIB(multio::LibMultio) << "Client config: " << conf << std::endl;
    for (auto&& cfg : conf.getSubConfigurations("plans")) {
        eckit::Log::debug<LibMultio>() << cfg << std::endl;

        const auto& plan = plans_.emplace_back(
            std::make_unique<action::Plan>(ComponentConfiguration(std::move(cfg), multioConfig())));

        if (planNames.find(plan->name()) != planNames.end()) {
            std::ostringstream oss;
            oss << "Plan names must be unique. The plan with name  \"" << plan->name() << "\" already exists";
            throw eckit::UserError(oss.str());
        }
        planNames.insert(plan->name());

        plan->matchedFields(activeSelectors_);
    }

    if (multioConfig().parsedConfig().has("active-matchers")) {
        for (const auto& m : multioConfig().parsedConfig().getSubConfigurations("active-matchers")) {
            std::map<std::string, std::set<std::string>> matches;
            for (const auto& k : m.keys()) {
                auto v = m.getStringVector(k);
                matches.emplace(k, std::set<std::string>(v.begin(), v.end()));
            }
        }
    }

#ifdef MULTIO_CLIENT_MEMORY_PROFILE_ENABLED
    tracer.startWriterThread();
#endif
}

MultioClient::MultioClient(MultioConfiguration&& multioConf) :
    MultioClient(getClientConf(multioConf), std::move(multioConf)) {}

MultioClient::MultioClient() : MultioClient(MultioConfiguration{}) {}

util::FailureHandlerResponse MultioClient::handleFailure(util::OnClientError t, const util::FailureContext& c,
                                                         util::DefaultFailureState&) const {
    // Last cascading instance, print nested contexts
    eckit::Log::error() << c;

    if (t == util::OnClientError::AbortAllTransports) {
        transport::TransportRegistry::instance().abortAll(c.eptr);
    }
    return util::FailureHandlerResponse::Rethrow;
};


void MultioClient::openConnections() {
    withFailureHandling([]() { transport::TransportRegistry::instance().openConnections(); },
                        []() { return std::string("MultioClient::openConnections"); });
}

void MultioClient::closeConnections() {
    withFailureHandling([]() { transport::TransportRegistry::instance().closeConnections(); },
                        []() { return std::string("MultioClient::closeConnections"); });
}

MultioClient::~MultioClient() {
    std::ofstream logFile{util::logfile_name(), std::ios_base::app};

    struct ::timeval tstamp;
    ::gettimeofday(&tstamp, 0);
    auto mSecs = tstamp.tv_usec;

    logFile << "MultioClient stops at " << eckit::DateTime{static_cast<double>(tstamp.tv_sec)}.time().now() << ":"
            << std::setw(6) << std::setfill('0') << mSecs;


    logFile << "\n ** Total wall-clock time spent in MultioClient " << eckit::Timing{totClientTimer_}.elapsed_ << "s"
            << std::endl;
}

void MultioClient::dispatch(message::Metadata metadata, eckit::Buffer&& payload, Message::Tag tag) {
    ASSERT(tag < Message::Tag::ENDTAG);
    dispatch(Message{Message::Header{tag, Peer{}, Peer{}, std::move(metadata)}, std::move(payload)});
}

void MultioClient::dispatch(message::Message msg) {
    withFailureHandling([&]() {
        if (msg.tag() == message::Message::Tag::Flush) {
            for (const auto& plan : plans_) {
                message::Metadata md = msg.metadata();
                md.set("clientPlanName", plan->name());

                plan->process(msg.modifyMetadata(std::move(md)));
            }
        }
        else {
            for (const auto& plan : plans_) {
                plan->process(msg);
            }
        }
    });

#ifdef MULTIO_CLIENT_MEMORY_PROFILE_ENABLED
    const auto current_time = std::chrono::system_clock::now();
    const auto elapsed_from_report = current_time - last_report_time;
    const auto elapsed_from_flush = current_time - last_flush_time;

    if (elapsed_from_report > tracerMemoryReportPeriod) {
        reportMemoryUsage();
        last_report_time = current_time;
    }

    if (elapsed_from_flush > tracerFlushPeriod) {
        tracer.flushCurrentChunk();
        last_flush_time = current_time;
    }
#endif
}

bool MultioClient::isFieldMatched(const message::Metadata& metadata) const {
    return activeSelectors_.matches(metadata);
}

}  // namespace multio::server
