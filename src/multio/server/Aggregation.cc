
#include "Aggregation.h"

#include <iostream>

#include "eckit/exception/Exceptions.h"

#include "atlas/array.h"
#include "atlas/util/Metadata.h"

#include "multio/server/Message.h"
#include "multio/server/SerialisationHelpers.h"

namespace multio {
namespace server {

Aggregation::Aggregation(std::vector<std::vector<int>> maps, const std::string& nm) :
    Action{nm},
    mappings_(std::move(maps)) {}

bool Aggregation::doExecute(Message& msg) const {

    auto field = unpack_atlas_field(msg);
    auto source = msg.peer();

    auto local_view = atlas::array::make_view<double, 1>(field);
    ASSERT(static_cast<size_t>(local_view.size()) == mappings_[source].size());

    // If we are not processing this field already, create it
    auto meta_str = pack_metadata(field.metadata());
    if (globals_.find(meta_str) == end(globals_)) {
        globals_[meta_str] = GlobalField{recreate_atlas_field(field.metadata())};
    }
    globals_[meta_str].noChunks++;

    // Aggregate
    auto view = atlas::array::make_view<double, 1>(globals_[meta_str].field);
    auto ii = 0;
    for (auto idx : mappings_[source]) {
        view(idx) = local_view(ii++);
    }

    auto ret = (globals_.at(meta_str).noChunks == mappings_.size());
    if (ret) {
        // Extract from map. What you really want is:
        // field = globals_.extract(meta_str); // Needs C++17
        field = std::move(globals_[meta_str].field);

        globals_.erase(meta_str);

        msg.rewind();
        atlas_field_to_message(field, msg);

        msg.rewind();
    }

    return ret;
}

Aggregation::GlobalField::GlobalField(atlas::Field fld) : field{std::move(fld)} {}

}  // namespace server
}  // namespace multio
