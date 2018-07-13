
#include <iostream>
#include <map>

#include "atlas/array.h"
#include "atlas/field/Field.h"
#include "atlas/util/Metadata.h"

#include "eckit/mpi/Parallel.h"
#include "eckit/testing/Test.h"

#include "multio/server/LocalPlan.h"
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
using TestFieldMap = std::map<std::string, std::vector<double>>;

namespace {
// Set up global data

const auto nServers = 2u;
const Transport& trans   = MpiTransport{"Test atlas field", nServers};

auto create_global_test_data() -> TestFieldMap {
    return {{"pi", {3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9, 3, 2, 3, 8, 4, 6, 2, 6, 4}},
            {"exp", {2, 7, 1, 8, 2, 8, 1, 8, 2, 8, 4, 5, 9, 0, 4, 5, 2, 3, 5, 3, 6, 0, 2, 8}}};
}

auto scatter_atlas_field(const TestField& gl_field) -> atlas::Field {
    if (trans.client()) {
        auto idxmap = create_local_to_global(field_size(), trans.no_clients(), trans.client_rank());
        return create_local_field(gl_field, idxmap, true);
    } else {
        return atlas::Field("dummy", atlas::array::DataType("real64"), make_shape(field_size()));
    }
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
    field_size()    = gl_field.at("pi").size();

    // Create fields from global data
    auto field_A = scatter_atlas_field(*gl_field.find("pi"));
    auto field_B = scatter_atlas_field(*gl_field.find("exp"));

    SECTION("created correctly") {
        if (trans.client()) {
            EXPECT(field_A.metadata().get<std::vector<int>>("mapping").size() == 4);
            EXPECT(field_B.metadata().get<std::vector<int>>("mapping").size() == 4);
        } else {
            EXPECT_THROWS(field_A.metadata().get<std::vector<int>>("mapping"));
            EXPECT_THROWS(field_B.metadata().get<std::vector<int>>("mapping"));
        }
    }

    SECTION("serialised successfully") {
        const auto no_client_proc = static_cast<int>(trans.no_clients());
        if (trans.client()) {
            // Send field_A as packed buffer
            auto dest = pack_atlas_field(field_A);
            comm().send<void>(dest.data(), dest.size(), no_client_proc, msg_tag::field_data);

            // Send field_B as packed buffer
            dest = pack_atlas_field(field_B);
            comm().send<void>(dest.data(), dest.size(), no_client_proc + 1, msg_tag::field_data);
        } else {
            auto counter = 0u;
            std::vector<double> received_field(field_size());
            do {  // Probe for atlas field
                auto status = comm().probe(comm().anySource(), comm().anyTag());
                // Query status
                auto chunk_size = comm().getCount<char>(status);

                std::vector<char> local_data(chunk_size);
                comm().receive<void>(local_data.data(), chunk_size, status.source(), status.tag());

                unpack_and_aggregate(local_data.data(), chunk_size, received_field);
            } while (++counter != trans.no_clients());

            eckit::Log::info() << "Rank = " << comm().rank() << ",    Received field:  ";
            print_buffer(received_field, eckit::Log::info());
            if (comm().rank() == trans.no_clients()) {
                EXPECT(gl_field.at("pi") == received_field);
            } else {
                EXPECT(gl_field.at("exp") == received_field);
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
