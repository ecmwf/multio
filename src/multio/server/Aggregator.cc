/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <fstream>

#include "Aggregator.h"

#include "multio/server/LocalIndices.h"
#include "multio/server/Message.h"
#include "multio/server/print_buffer.h"

#include "eckit/log/Log.h"

namespace multio {
namespace server {

Aggregator::Aggregator(size_t gl_size, size_t nb_fields) :
    global_size_(gl_size),
    nb_fields_(nb_fields) {}

eckit::Buffer Aggregator::gather(const std::vector<Message>& msgs, const Mapping& maps) {
    std::vector<double> global_field(global_size_);
    for (auto msg : msgs) {
        auto data_ptr = static_cast<const double*>(msg.payload().data());
        std::vector<double> local_field(data_ptr, data_ptr + msg.size() / sizeof(double));

        maps.at(msg.source())->to_global(local_field, global_field);
    }

    // Yet another unnecessary copy; you could work directly on the buffer above
    return eckit::Buffer{reinterpret_cast<char*>(global_field.data()),
                         global_size_ * sizeof(double)};
}

std::vector<Message> scatter(const Message& msg, const Mapping& maps) {
    std::vector<Message> msgs;

    auto data_ptr = static_cast<const double*>(msg.payload().data());
    std::vector<double> global_field(data_ptr, data_ptr + msg.size() / sizeof(double));
    for (const auto& map : maps) {
        std::vector<double> local_field;
        map.second->to_local(global_field, local_field);
        msgs.emplace_back(Message::Header{msg.tag(), Peer{}, Peer{}, msg.mapping(), msg.map_count(),
                                          msg.category(), msg.field_id(), msg.field_size()},
                          eckit::Buffer{reinterpret_cast<char*>(local_field.data()),
                                        local_field.size() * sizeof(double)});
    }

    return msgs;
}

}  // namespace server
}  // namespace multio
