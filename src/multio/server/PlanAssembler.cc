
#include "PlanAssembler.h"

#include <fstream>
#include <functional>

#include "eckit/io/Buffer.h"

#include "atlas/util/Metadata.h"

#include "multio/DataSink.h"

#include "multio/server/Action.h"
#include "multio/server/Aggregation.h"
#include "multio/server/Sink.h"

#include "multio/server/Message.h"
#include "multio/server/Plan.h"
#include "multio/server/PlanConfigurations.h"
#include "multio/server/SerialisationHelpers.h"

namespace multio {
namespace server {

PlanAssembler::PlanAssembler() : planConfigs_{plan_configurations()} {}

bool PlanAssembler::tryCreate(const Message& msg) {
    ASSERT(msg.tag() == msg_tag::plan_data);

    // Query plan's metadata
    auto meta_size = 0ul;
    msg.read(&meta_size, sizeof(unsigned long));

    auto meta_buf = eckit::Buffer(meta_size);
    msg.read(meta_buf, meta_size);

    auto metadata = atlas::util::Metadata{unpack_metadata(meta_buf)};
    auto plan_name = metadata.get<std::string>("plan_name");
    if (!planConfigs_.has(plan_name)) {
        ASSERT(false);
    }
    auto config = atlas::util::Metadata{planConfigs_.getSubConfiguration(plan_name)};

    if (config.get<std::string>("aggregation") == "none") {
        // Nothing to cache in the factory -- delegate creation to the handOver method
        return true;
    }

    if (plansBeingProcessed_.find(plan_name) == end(plansBeingProcessed_)) {
        // Create new plan and, depending on the type of plan, return it or put it into
        // plansBeingProcessed_;
        plansBeingProcessed_[plan_name] =
            std::vector<std::vector<int>>(metadata.get<size_t>("no_maps"));
    }

    auto data_size = msg.size() - meta_size - sizeof(unsigned long);
    auto& local_map = plansBeingProcessed_[plan_name][msg.peer()];
    ASSERT(local_map.empty());
    local_map.resize(data_size / sizeof(int));
    ASSERT(!local_map.empty());
    msg.read(local_map.data(), data_size);
    return isComplete(plan_name);
}

Plan PlanAssembler::handOver(const std::string& plan_name) {
    std::unique_ptr<Action> root = nullptr;
    auto config = atlas::util::Metadata{planConfigs_.getSubConfiguration(plan_name)};

    // Create aggregation
    if (config.get<std::string>("aggregation") == "indexed") {
        root.reset(
            new Aggregation{std::move(plansBeingProcessed_[plan_name]), "Indexed aggregation"});
    }
    else {
        ASSERT(false);
    }

    if (config.get<std::string>("encoding") != "none") {
        ASSERT(false);  // Not yet implemented
    }

    if (config.get<std::string>("multio_sink") == "file") {
        eckit::LocalConfiguration config;
        config.set("path", eckit::PathName{"/dev/null"});  // Default to black hole
        root->add(
            std::unique_ptr<Action>{new Sink{DataSinkFactory::instance().build("file", config)}});
    }
    // Create further actions...

    return Plan{plan_name, std::move(root)};
}

bool PlanAssembler::isComplete(const std::string& plan_name) const {
    const auto& maps = plansBeingProcessed_.at(plan_name);
    return none_of(begin(maps), end(maps), mem_fn(&std::vector<int>::empty));
}

}  // namespace server
}  // namespace multio
