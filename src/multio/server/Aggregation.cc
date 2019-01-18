
#include "Aggregation.h"

#include <iostream>

#include "eckit/exception/Exceptions.h"

#include "atlas/array.h"
#include "atlas/util/Metadata.h"

#include "multio/server/Message.h"
#include "multio/server/print_buffer.h"
#include "multio/server/SerialisationHelpers.h"

namespace multio {
namespace server {

Aggregation::Aggregation(std::vector<std::vector<int>> maps, const std::string& nm) :
    Action{nm},
    mappings_(std::move(maps)) {}

bool Aggregation::doExecute(std::shared_ptr<Message> msg) const {

    auto meta_str = pack_metadata(fetch_metadata(*msg));

    messages_[meta_str].push_back(msg);

    auto ret = messages_.at(meta_str).size() == mappings_.size();
    if (ret) {

        auto global_field = aggregate(meta_str);
        messages_.erase(meta_str);

        msg->rewind();
        atlas_field_to_message(global_field, *msg);

        msg->rewind();
    }

    return ret;
}

atlas::Field Aggregation::aggregate(const std::string& meta_str) const {

    auto global_field = recreate_atlas_field(atlas::util::Metadata{unpack_metadata(meta_str)});

    auto global_view = atlas::array::make_view<double, 1>(global_field);
    for (auto msg : messages_.at(meta_str)) {

        auto local_field = unpack_atlas_field(*msg);
        auto local_view = atlas::array::make_view<double, 1>(local_field);

        auto ii = 0;
        for (auto idx : mappings_[msg->peer()]) {
            global_view(idx) = local_view(ii++);
        }
    }

    return global_field;
}


}  // namespace server
}  // namespace multio
