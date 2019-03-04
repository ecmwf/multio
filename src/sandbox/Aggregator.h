/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany
/// @author Simon Smart
/// @author Tiago Quintino

/// @date Mar 2019

#ifndef multio_sandbox_Aggregator_H
#define multio_sandbox_Aggregator_H

#include <vector>

#include "sandbox/Mappings.h"

namespace eckit {
class Buffer;
}

namespace multio {
namespace sandbox {

class Aggregator {
public:
    Aggregator(size_t gL_size, size_t nb_fields);

    eckit::Buffer gather(const std::vector<Message>& msg, const Mapping& maps);

    std::vector<Message> scatter(Message&& msg, const Mapping& maps);

private:
    size_t global_size_;
    size_t nb_fields_;
};

}  // namespace sandbox
}  // namespace multio

#endif
