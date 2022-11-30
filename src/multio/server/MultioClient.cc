
#include "MultioClient.h"

#include <algorithm>
#include <fstream>
#include <iomanip>

#include "eckit/config/LocalConfiguration.h"
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

MultioClient::MultioClient(const ClientConfigurationContext& confCtx) : FailureAware(confCtx) {
    ASSERT(confCtx.componentTag() == util::ComponentTag::Client);
    totClientTimer_.start();

    std::ofstream logFile{util::logfile_name(), std::ios_base::app};

    struct ::timeval tstamp;
    ::gettimeofday(&tstamp, 0);
    auto mSecs = tstamp.tv_usec;

    logFile << "MultioClient starts at " << eckit::DateTime{static_cast<double>(tstamp.tv_sec)}.time().now() << ":"
            << std::setw(6) << std::setfill('0') << mSecs << " -- ";


    LOG_DEBUG_LIB(multio::LibMultio) << "Client config: " << confCtx.config() << std::endl;
    for (auto&& cfg : confCtx.subContexts("plans", ComponentTag::Plan)) {
        eckit::Log::debug<LibMultio>() << cfg.config() << std::endl;
        plans_.emplace_back(new action::Plan(std::move(cfg)));
        plans_.back()->matchedFields(activeSelectors_);
    }

    if (confCtx.globalConfig().has("active-matchers")) {
        for (const auto& m : confCtx.globalConfig().getSubConfigurations("active-matchers")) {
            std::map<std::string, std::set<std::string>> matches;
            for (const auto& k : m.keys()) {
                auto v = m.getStringVector(k);
                matches.emplace(k, std::set<std::string>(v.begin(), v.end()));
            }
        }
    }
}

util::FailureHandlerResponse MultioClient::handleFailure(util::OnClientError t, const util::FailureContext& c,
                                                         util::DefaultFailureState&) const {
    // Last cascading instance, print nested contexts
    print(eckit::Log::error(), c);

    if (t == util::OnClientError::AbortAllTransports) {
        transport::TransportRegistry::instance().abortAll();
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
        for (const auto& plan : plans_) {
            plan->process(msg);
        }
    });
}

bool MultioClient::isFieldMatched(const message::Metadata& metadata) const {
    return activeSelectors_.matches(metadata);
}

}  // namespace server
}  // namespace multio
