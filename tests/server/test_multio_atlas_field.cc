
#include <iostream>
#include <map>

#include "atlas/array.h"
#include "atlas/field/Field.h"
#include "atlas/util/Metadata.h"

#include "eckit/mpi/Parallel.h"
#include "eckit/testing/Test.h"

#include "multio/server/PartialMapping.h"
#include "multio/server/MpiTransport.h"
#include "multio/server/msg_tag.h"
#include "multio/server/print_buffer.h"
#include "multio/server/SerialisationHelpers.h"

#include "TestServerHelpers.h"

using namespace eckit::testing;

namespace multio {
namespace server {
namespace test {

using atlas::array::make_shape;
using eckit::mpi::comm;
using TestFieldMap = std::map<std::string, atlas::Field>;

namespace {

// Set up global data

const Transport& transport() {
    static const Transport& transport = MpiTransport{"Test atlas field", 2};
    return transport;
}

auto create_global_test_data() -> TestFieldMap {
    TestFieldMap test_fields;

    std::vector<double> v{3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9, 3, 2, 3, 8, 4, 6, 2, 6, 4};
    test_fields["pi"] = atlas::Field("pi", atlas::array::DataType("real64"), make_shape(v.size()));
    auto view = atlas::array::make_view<double, 1>(test_fields["pi"]);
    copy(begin(v), end(v), view.data());

    v = {2, 7, 1, 8, 2, 8, 1, 8, 2, 8, 4, 5, 9, 0, 4, 5, 2, 3, 5, 3, 6, 0, 2, 8};
    test_fields["exp"] =
        atlas::Field("exp", atlas::array::DataType("real64"), make_shape(v.size()));
    view = atlas::array::make_view<double, 1>(test_fields["exp"]);
    copy(begin(v), end(v), view.data());

    return test_fields;
}

auto scatter_atlas_field(const atlas::Field& gl_field) -> atlas::Field {
    if (transport().client()) {
        auto idxmap =
            create_partial_mapping(field_size(), transport().noClients(), transport().clientRank());
        return create_local_field(gl_field, idxmap, true);
    }
    else {
        return atlas::Field("dummy", atlas::array::DataType("real64"), make_shape(field_size()));
    }
}

inline auto get_data(const atlas::Field& field) -> std::vector<double> {
    auto vec = std::vector<double>{};

    auto view = atlas::array::make_view<double, 1>(field);
    copy_n(view.data(), view.size(), back_inserter(vec));

    return vec;
}

void unpack_and_aggregate(char* local_data, size_t sz, std::vector<double>& received_field) {
    auto meta_size = *reinterpret_cast<unsigned long*>(local_data);
    auto pos = sizeof(unsigned long);

    atlas::util::Metadata metadata = unpack_metadata(std::string(&local_data[pos], meta_size));
    pos += meta_size;

    auto local_map = metadata.get<std::vector<size_t>>("mapping");

    auto data_size = (sz - meta_size - sizeof(long)) / sizeof(double);

    EXPECT(data_size == local_map.size());

    // Aggregate
    for (auto ii = 0u; ii != local_map.size(); ++ii) {
        received_field[local_map[ii]] = *(reinterpret_cast<double*>(&local_data[pos]) + ii);
    }
}
}  // namespace

CASE("Test that atlas field is ") {
    auto gl_field = create_global_test_data();
    field_size() = gl_field.at("pi").size();

    // Create fields from global data
    auto field_A = scatter_atlas_field(gl_field.at("pi"));
    auto field_B = scatter_atlas_field(gl_field.at("exp"));

    SECTION("created correctly") {
        if (transport().client()) {
            EXPECT(field_A.metadata().get<std::vector<int>>("mapping").size() == 4);
            EXPECT(field_B.metadata().get<std::vector<int>>("mapping").size() == 4);
        }
        else {
            EXPECT_THROWS(field_A.metadata().get<std::vector<int>>("mapping"));
            EXPECT_THROWS(field_B.metadata().get<std::vector<int>>("mapping"));
        }
    }

    SECTION("serialised successfully") {
        const auto no_client_proc = static_cast<int>(transport().noClients());
        if (transport().client()) {
            // Send field_A as packed buffer
            auto dest = pack_atlas_field(field_A);
            comm().send<void>(dest.data(), dest.size(), no_client_proc, msg_tag::field_data);

            // Send field_B as packed buffer
            dest = pack_atlas_field(field_B);
            comm().send<void>(dest.data(), dest.size(), no_client_proc + 1, msg_tag::field_data);
        }
        else {
            auto counter = 0u;
            std::vector<double> received_field(field_size());
            do {  // Probe for atlas field
                auto status = comm().probe(comm().anySource(), comm().anyTag());
                // Query status
                auto chunk_size = comm().getCount<char>(status);

                std::vector<char> local_data(chunk_size);
                comm().receive<void>(local_data.data(), chunk_size, status.source(), status.tag());

                unpack_and_aggregate(local_data.data(), chunk_size, received_field);
            } while (++counter != transport().noClients());

            if (comm().rank() == transport().noClients()) {
                EXPECT(get_data(gl_field.at("pi")) == received_field);
            }
            else {
                EXPECT(get_data(gl_field.at("exp")) == received_field);
            }
        }
    }
}

//-----------------------------------------------------------------------------

}  // namespace server
}  // namespace test
}  // namespace multio

int main(int argc, char** argv) {
    return run_tests(argc, argv);
}
