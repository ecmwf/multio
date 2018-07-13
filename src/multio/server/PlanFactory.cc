
#include "PlanFactory.h"

#include <functional>

#include "eckit/io/Buffer.h"

#include "atlas/util/Metadata.h"

#include "multio/DataSink.h"

#include "multio/server/Action.h"
#include "multio/server/Aggregation.h"
#include "multio/server/Sink.h"

#include "multio/server/Message.h"
#include "multio/server/Plan.h"
#include "multio/server/SerialisationHelpers.h"

namespace multio {
namespace server {

PlanFactory::PlanFactory(size_t no_maps) : no_maps_(no_maps) {}

bool PlanFactory::try_create(const Message& msg) {
    ASSERT(msg.tag() == msg_tag::plan_data);

    // Query plan's metadata
    auto meta_size = 0ul;
    msg.read(&meta_size, sizeof(unsigned long));

    auto meta_buf = eckit::Buffer(meta_size);
    msg.read(meta_buf, meta_size);

    auto metadata = atlas::util::Metadata{unpack_metadata(meta_buf)};
    auto plan_name = metadata.get<std::string>("name");

    if (metadata.get<std::string>("aggregation") != "indexed") {
        // Nothing to cache in the factory -- delegate creation to the handOver method
        return true;
    }

    if (plans_being_processed_.find(plan_name) == end(plans_being_processed_)) {
        // Create new plan and, depending on the type of plan, return it or put it into
        // plans_being_processed_;
        plans_being_processed_[plan_name] = std::vector<std::vector<int>>(no_maps_);
    }
    auto data_size = msg.size() - meta_size - sizeof(unsigned long);
    auto& local_map = plans_being_processed_[plan_name][msg.peer()];
    ASSERT(local_map.empty());
    local_map.resize(data_size / sizeof(int));
    ASSERT(!local_map.empty());
    msg.read(local_map.data(), data_size);
    return isComplete(plan_name);
}

Plan PlanFactory::handOver(const std::string& plan_name) {
    ActionList actions;

    // Create aggregation
    actions.emplace_back(new Aggregation{std::move(plans_being_processed_[plan_name]), "Indexed"});

    actions.emplace_back(new Sink{nullptr});

    // Create further actions...

    return Plan{plan_name, std::move(actions)};
}

bool PlanFactory::isComplete(const std::string& plan_name) const {
    const auto& maps = plans_being_processed_.at(plan_name);
    return none_of(begin(maps), end(maps), mem_fn(&std::vector<int>::empty));
}

}  // namespace server
}  // namespace multio
