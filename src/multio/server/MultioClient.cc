
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
    auto activeFieldInserter = std::inserter(activeFields_, activeFields_.end());
    auto activeCategoryInserter = std::inserter(activeCategories_, activeCategories_.end());
    for (auto&& cfg : confCtx.subContexts("plans", ComponentTag::Plan)) {
        eckit::Log::debug<LibMultio>() << cfg.config() << std::endl;
        plans_.emplace_back(new action::Plan(std::move(cfg)));
        plans_.back()->computeActiveFields(activeFieldInserter);
        plans_.back()->computeActiveCategories(activeCategoryInserter);
    }
    if (confCtx.globalConfig().has("active-fields")) {
        const auto& vec = confCtx.globalConfig().getStringVector("active-fields");
        std::copy(vec.begin(), vec.end(), activeFieldInserter);
    }
}

util::FailureHandlerResponse MultioClient::handleFailure(util::OnClientError t, const util::FailureContext& c, util::DefaultFailureState&) const {
    // Last cascading instance, print nested contexts
    print(c);
    
    if (t == util::OnClientError::AbortAllTransports) {
        transport::TransportRegistry::instance().abortAll();
    }
    return util::FailureHandlerResponse::Rethrow;
};


void MultioClient::openConnections() {
    withFailureHandling([&]() { transport::TransportRegistry::instance().openConnections(); },
                        []() { return std::string("MultioClient::openConnections"); });
}

void MultioClient::closeConnections() {
    withFailureHandling([&]() { transport::TransportRegistry::instance().closeConnections(); },
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

bool MultioClient::isFieldActive(const std::string& name) const {
    return activeFields_.find(name) != end(activeFields_);
}

bool MultioClient::isCategoryActive(const std::string& name) const {
    return activeCategories_.find(name) != end(activeCategories_);
}

}  // namespace server
}  // namespace multio
