/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Listener.h"

#include <unistd.h>

#include <fstream>
#include <functional>
#include <typeinfo>

#include "eckit/config/Resource.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/log/ResourceUsage.h"

#include "multio/LibMultio.h"
#include "multio/message/Message.h"

#include "multio/server/Dispatcher.h"
#include "multio/transport/Transport.h"
#include "multio/transport/TransportRegistry.h"
#include "multio/util/ScopedThread.h"

#ifdef MULTIO_SERVER_MEMORY_PROFILE_ENABLED

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

}

#endif

namespace multio::server {

using message::Message;
using transport::Transport;
using util::ScopedThread;

Listener::Listener(const config::ComponentConfiguration& compConf, Transport& trans) :
    FailureAware(compConf),
    dispatcher_{std::make_unique<Dispatcher>(compConf, msgQueue_)},
    transport_{trans},
    clientCount_{transport_.clientPeers().size()},
    msgQueue_(eckit::Resource<size_t>("multioMessageQueueSize;$MULTIO_MESSAGE_QUEUE_SIZE", 1024 * 1024)) {}

Listener::~Listener() = default;

util::FailureHandlerResponse Listener::handleFailure(util::OnReceiveError t, const util::FailureContext& c,
                                                     util::DefaultFailureState&) const {
    msgQueue_.interrupt(c.eptr);
    transport::TransportRegistry::instance().abortAll(c.eptr);

    return util::FailureHandlerResponse::Rethrow;
};

void Listener::start() {

    eckit::ResourceUsage usage{"multio listener"};

    // Store thread errors
    ScopedThread lstnThread{std::thread{[&]() { this->listen(); }}};

    ScopedThread dpatchThread{std::thread{[&]() { dispatcher_->dispatch(); }}};

    withFailureHandling([&]() {
        do {
            Message msg = transport_.receive();

            switch (msg.tag()) {
                case Message::Tag::Open:
                    connections_.insert(msg.source());
                    ++openedCount_;
                    LOG_DEBUG_LIB(LibMultio)
                        << "*** OPENING connection to " << msg.source() << ":    client count = " << clientCount_
                        << ", opened count = " << openedCount_ << ", active connections = " << connections_.size()
                        << std::endl;
                    break;

                case Message::Tag::Close:
                    connections_.erase(connections_.find(msg.source()));
                    LOG_DEBUG_LIB(LibMultio)
                        << "*** CLOSING connection to " << msg.source() << ":    client count = " << clientCount_
                        << ", opened count = " << openedCount_ << ", active connections = " << connections_.size()
                        << std::endl;
                    break;

                case Message::Tag::Domain:
                case Message::Tag::Mask:
                case Message::Tag::Notification:
                case Message::Tag::Flush:
                case Message::Tag::Field:
                    checkConnection(msg.source());
                    LOG_DEBUG_LIB(LibMultio) << "*** Message received: " << msg << std::endl;
                    msgQueue_.emplace(std::move(msg));
                    break;

                default:
                    std::ostringstream oss;
                    oss << "Unhandled message: " << msg << std::endl;
                    throw eckit::SeriousBug(oss.str());
            }
        } while (moreConnections() && msgQueue_.checkInterrupt());
    });

    LOG_DEBUG_LIB(LibMultio) << "*** STOPPED listening loop " << std::endl;

    msgQueue_.close();

    LOG_DEBUG_LIB(LibMultio) << "*** CLOSED message queue " << std::endl;
}

void Listener::listen() {
    withFailureHandling([this]() {

#ifdef MULTIO_SERVER_MEMORY_PROFILE_ENABLED
        tracer.startWriterThread();

        auto last_report_time = std::chrono::system_clock::now();
        auto last_flush_time = std::chrono::system_clock::now();
#endif

        do {
            transport_.listen();

#ifdef MULTIO_SERVER_MEMORY_PROFILE_ENABLED
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
        } while (msgQueue_.checkInterrupt() && !msgQueue_.closed());
    });
}

bool Listener::moreConnections() const {
    return !connections_.empty() || openedCount_ != clientCount_;
}

void Listener::checkConnection(const message::Peer& conn) const {
    if (connections_.find(conn) == end(connections_)) {
        std::ostringstream oss;
        oss << "Connection to " << conn << " is not open";
        throw eckit::SeriousBug{oss.str(), Here()};
    }
}

}  // namespace multio::server
