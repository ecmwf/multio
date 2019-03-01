/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Aggregator.h"

#include "sandbox/Message.h"

#include "eckit/log/Log.h"

namespace multio {
namespace sandbox {

Aggregator::Aggregator(size_t gl_size, size_t nb_fields) :
    global_size_(gl_size),
    nb_fields_(nb_fields) {}

eckit::Buffer Aggregator::aggregate(const std::vector<Message>& msgs, const Mapping& maps) {
    std::vector<double> global_field(global_size_);
    for (auto msg : msgs) {
        auto data_ptr = static_cast<double*>(msg.payload().data());
        std::vector<double> local_field(data_ptr, data_ptr + msg.size() / sizeof(double));

        auto ii = 0u;
        for (auto idx : maps.at(msg.source())) {
            global_field[idx] = local_field[ii++];
        }
    }

    // Yet another unnecessary copy; you could work directly on the buffer above
    return eckit::Buffer{reinterpret_cast<char*>(global_field.data()),
                         global_size_ * sizeof(double)};
}

std::vector<Message> scatter(const Message& msg, const Mapping& maps) {}

}  // namespace sandbox
}  // namespace multio
