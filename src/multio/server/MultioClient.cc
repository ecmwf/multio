
#include "MultioClient.h"

#include <algorithm>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/config/Resource.h"
#include "eckit/config/YAMLConfiguration.h"
#include "eckit/filesystem/PathName.h"

#include "multio/LibMultio.h"
#include "multio/message/Message.h"
#include "multio/transport/TransportRegistry.h"

using multio::message::Message;
using multio::message::Peer;

namespace multio {
namespace server {

MultioClient::MultioClient(const eckit::Configuration& config) {
    LOG_DEBUG_LIB(multio::LibMultio) << "Client config: " << config << std::endl;
    const std::vector<eckit::LocalConfiguration> plans =
        config.getSubConfiguration("client").getSubConfigurations("plans");
    for (const auto& cfg : plans) {
        eckit::Log::debug<LibMultio>() << cfg << std::endl;
        plans_.emplace_back(new action::Plan(cfg));
    }
}

void MultioClient::openConnections() {
    transport::TransportRegistry::instance().openConnections();
}

void MultioClient::closeConnections() {
    transport::TransportRegistry::instance().closeConnections();
}

MultioClient::~MultioClient() = default;

void MultioClient::dispatch(message::Metadata metadata, eckit::Buffer&& payload, int itag) {
    auto tag = static_cast<Message::Tag>(itag);
    ASSERT(tag < Message::Tag::ENDTAG);
    Message msg{Message::Header{tag, Peer{}, Peer{}, std::move(metadata)},
                std::move(payload)};

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
