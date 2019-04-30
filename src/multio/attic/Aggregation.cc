
#include "Aggregation.h"

#include <iostream>

#include "eckit/exception/Exceptions.h"

#include "atlas/array.h"
#include "atlas/util/Metadata.h"

#include "multio/attic/Mappings.h"
#include "multio/attic/Message.h"
#include "multio/attic/print_buffer.h"
#include "multio/attic/SerialisationHelpers.h"

namespace multio {
namespace attic {

Aggregation::Aggregation(const std::string& nm) : Action{nm} {}

bool Aggregation::doExecute(std::shared_ptr<Message> msg) const {

    if(msg->tag() == msg_tag::step_complete) {
        return true;
    }

    auto metadata = fetch_metadata(*msg);

    map_name_ = metadata.get<std::string>("mapping");

    auto meta_str = pack_metadata(metadata);

    messages_[meta_str].push_back(msg);

    auto ret = messages_.at(meta_str).size() == Mappings::instance().get(map_name_).size();
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
        for (auto idx : Mappings::instance().get(map_name_)[msg->peer()]) {
            global_view(idx) = local_view(ii++);
        }
    }

    return global_field;
}

}  // namespace attic
}  // namespace multio
