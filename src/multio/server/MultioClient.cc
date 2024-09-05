
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

using multio::message::Message;
using multio::message::Peer;

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
    throw eckit::UserError(oss.str(), Here());
}

}  // namespace

MultioClient::MultioClient(const eckit::LocalConfiguration& conf, MultioConfiguration&& multioConf) :
    MultioConfigurationHolder(std::move(multioConf), config::LocalPeerTag::Client),
    FailureAware(ComponentConfiguration(conf, multioConfig())) {
    totClientTimer_.start();

    plans_ = action::Plan::makePlans(conf.getSubConfigurations("plans"), multioConfig(), activeSelectors_);

    // TODO: Put the whole plan list in a separate class and make this logic reusable
    std::unordered_set<std::string> planNames;
    for (const auto& plan : plans_) {
        if (planNames.find(plan->name()) != std::cend(planNames)) {
            std::ostringstream oss;
            oss << "Plan names must be unique. The plan with name  \"" << plan->name() << "\" already exists";
            throw eckit::UserError(oss.str());
        }
        planNames.insert(plan->name());
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
}

MultioClient::MultioClient(MultioConfiguration&& multioConf) :
    MultioClient(getClientConf(multioConf), std::move(multioConf)) {}

MultioClient::MultioClient() : MultioClient(MultioConfiguration{}) {}

MultioClient::~MultioClient() = default;

util::FailureHandlerResponse MultioClient::handleFailure(util::OnClientError t, const util::FailureContext& c,
                                                         util::DefaultFailureState&) const {
    // Last cascading instance, print nested contexts
    eckit::Log::error() << c;

    if (t == util::OnClientError::AbortTransport) {
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

void MultioClient::dispatch(message::SharedMetadata metadata, eckit::Buffer&& payload, Message::Tag tag) {
    ASSERT(tag < Message::Tag::ENDTAG);
    dispatch(Message{Message::Header{tag, Peer{}, Peer{}, std::move(metadata)}, std::move(payload)});
}

void MultioClient::dispatch(message::SharedMetadata metadata, const message::PayloadReference& payload,
                            Message::Tag tag) {
    ASSERT(tag < Message::Tag::ENDTAG);
    dispatch(Message{Message::Header{tag, Peer{}, Peer{}, std::move(metadata)}, payload});
}

void MultioClient::dispatch(message::Message msg) {
    withFailureHandling([&]() {
        if (msg.tag() == message::Message::Tag::Flush) {
            for (const auto& plan : plans_) {
                message::Message msg2{msg};
                msg2.acquireMetadata();
                msg2.modifyMetadata().set("clientPlanName", plan->name());

                plan->process(std::move(msg2));
            }
        }
        else {
            for (const auto& plan : plans_) {
                plan->process(msg);
            }
        }
    });
}

bool MultioClient::isFieldMatched(const message::Metadata& metadata) const {
    return activeSelectors_.matches(metadata);
}

}  // namespace multio::server
