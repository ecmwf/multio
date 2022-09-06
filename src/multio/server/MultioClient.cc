
#include "MultioClient.h"

#include <algorithm>
#include <fstream>
#include <iomanip>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/config/Resource.h"
#include "eckit/config/YAMLConfiguration.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/log/Statistics.h"
#include "eckit/types/DateTime.h"

#include "multio/LibMultio.h"
#include "multio/message/Message.h"
#include "multio/transport/TransportRegistry.h"
#include "multio/util/logfile_name.h"

using multio::message::Message;
using multio::message::Peer;

namespace multio {
namespace server {

MultioClient::MultioClient(const ClientConfigurationContext& confCtx) {
    totClientTimer_.start();

    std::ofstream logFile{util::logfile_name(), std::ios_base::app};

    struct ::timeval tstamp;
    ::gettimeofday(&tstamp, 0);
    auto mSecs = tstamp.tv_usec;

    logFile << "MultioClient starts at "
            << eckit::DateTime{static_cast<double>(tstamp.tv_sec)}.time().now() << ":"
            << std::setw(6) << std::setfill('0') << mSecs << " -- ";


    LOG_DEBUG_LIB(multio::LibMultio) << "Client config: " << confCtx.config() << std::endl;
    for (auto&& cfg : confCtx.subContext("client").subContexts("plans")) {
        eckit::Log::debug<LibMultio>() << cfg.config() << std::endl;
        plans_.emplace_back(new action::Plan(std::move(cfg)));
    }
}

void MultioClient::openConnections() {
    transport::TransportRegistry::instance().openConnections();
}

void MultioClient::closeConnections() {
    transport::TransportRegistry::instance().closeConnections();
}

MultioClient::~MultioClient() {
    std::ofstream logFile{util::logfile_name(), std::ios_base::app};

    struct ::timeval tstamp;
    ::gettimeofday(&tstamp, 0);
    auto mSecs = tstamp.tv_usec;

    logFile << "MultioClient stops at "
            << eckit::DateTime{static_cast<double>(tstamp.tv_sec)}.time().now() << ":"
            << std::setw(6) << std::setfill('0') << mSecs;


    logFile << "\n ** Total wall-clock time spent in MultioClient "
            << eckit::Timing{totClientTimer_}.elapsed_ << "s" <<std::endl;
}

void MultioClient::dispatch(message::Metadata metadata, eckit::Buffer&& payload, Message::Tag tag) {
    ASSERT(tag < Message::Tag::ENDTAG);
    Message msg{Message::Header{tag, Peer{}, Peer{}, std::move(metadata)}, std::move(payload)};

    for (const auto& plan : plans_) {
        plan->process(msg);
    }
}

void MultioClient::dispatch(message::Message msg) {
    for (const auto& plan : plans_) {
        plan->process(msg);
    }
}

}  // namespace server
}  // namespace multio
