/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#ifndef multio_TestServerHelpers_H
#define multio_TestServerHelpers_H

#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include "eckit/filesystem/TmpFile.h"
#include "eckit/mpi/Comm.h"

#include "atlas/array.h"
#include "atlas/field/Field.h"
#include "atlas/util/Metadata.h"

#include "multio/server/PartialMapping.h"
#include "multio/server/Message.h"
#include "multio/server/SerialisationHelpers.h"

#include "create_random_data.h"
#include "../TestHelpers.h"

namespace multio {
namespace server {
namespace test {

using atlas::array::make_shape;
using atlas::util::Metadata;
using eckit::mpi::comm;

// Default global values, but each test is allowed to override them
inline size_t& field_size() {
    static size_t val;
    return (!val ? (val = 23) : val);
}

inline size_t& root() {
    static size_t rt;  // = 0 if not set to another value
    return rt;
}

inline void set_metadata(Metadata& metadata, const std::string& name, int level, int step) {
    metadata.set("name", name);
    metadata.set("plan_name", "atm_grid");
    metadata.set("gl_size", field_size());
    metadata.set("levels", level);
    metadata.set("steps", step);
}

// TODO: This function still assumes that MPI is used in the test for the transport layer. We will
// have to change this if want to use it for testing other trnasport layers
inline auto create_global_test_field(const Metadata& metadata, const size_t sz) -> atlas::Field {
    auto name = metadata.get<std::string>("name");
    auto vals = (comm().rank() == root()) ? create_random_data(name, sz) : std::vector<double>(sz);
    comm().broadcast(vals, root());

    auto field = atlas::Field(name, atlas::array::DataType("real64"), make_shape(field_size()));
    auto view = atlas::array::make_view<double, 1>(field);
    copy(begin(vals), end(vals), view.data());

    field.metadata() = metadata;
    return field;
}

inline auto set_up_atlas_test_field(const std::string& name) -> atlas::Field {
    atlas::util::Metadata metadata;
    set_metadata(metadata, name, 850, 1);

    auto field = create_global_test_field(metadata, field_size());

    field.metadata() = metadata;

    return field;
}

inline atlas::Field create_local_field(const atlas::Field& gl_field, const std::vector<int>& idxmap,
                                       bool add_map_to_metadata = false) {
    auto local_field =
        atlas::Field(gl_field.name(), atlas::array::DataType("real64"), make_shape(idxmap.size()));
    local_field.metadata() = gl_field.metadata();
    if (add_map_to_metadata) {
        local_field.metadata().set("mapping", idxmap);
    }
    auto view_global = atlas::array::make_view<double, 1>(gl_field);
    auto view_local = atlas::array::make_view<double, 1>(local_field);
    auto ii = 0;
    for (auto idx : idxmap) {
        view_local(ii++) = view_global(idx);
    }
    return local_field;
}

inline auto unpack_mapping(Message& msg) -> PartialMapping {
    auto meta_size = 0ul;
    msg.read(&meta_size, sizeof(unsigned long));

    auto meta_buf = eckit::Buffer(meta_size);
    msg.read(meta_buf, meta_size);

    auto metadata = atlas::util::Metadata{unpack_metadata(meta_buf)};
    auto mapping = PartialMapping{metadata.get<std::string>("plan_name")};

    auto data_size = msg.size() - meta_size - sizeof(long);
    std::vector<int> local_map(data_size / sizeof(int));
    mapping.indices.resize(data_size / sizeof(int));

    msg.read(mapping.indices.data(), data_size);

    return mapping;
}

inline bool operator==(const PartialMapping& lhs, const PartialMapping& rhs) {
    return (pack_metadata(lhs.metadata) == pack_metadata(rhs.metadata)) &&
           (lhs.indices == rhs.indices);
}

}  // namespace server
}  // namespace test
}  // namespace multio


#endif  // multio_TestServerHelpers_H
