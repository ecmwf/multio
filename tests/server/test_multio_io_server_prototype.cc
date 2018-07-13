
#include "eckit/testing/Test.h"

#include "atlas/array.h"
#include "atlas/field/Field.h"
#include "atlas/util/Metadata.h"

#include "multio/server/Dispatcher.h"
#include "multio/server/Distributor.h"
#include "multio/server/LocalPlan.h"
#include "multio/server/MpiTransport.h"
#include "multio/server/SerialisationHelpers.h"

#include "TestServerHelpers.h"

using namespace eckit::testing;
using atlas::array::make_shape;
using multio::test::file_content;

namespace multio {
namespace server {
namespace test {

using TestFieldSet = std::vector<TestField>;

namespace {

const auto nServers = 3u;
const Transport& transport = MpiTransport{"i/o server test", nServers};

auto global_fields = std::vector<atlas::Field>{};

auto create_global_test_data(const size_t sz) -> TestFieldSet {
    TestFieldSet gl_fields;

    for (auto step : {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16}) {
        for (auto level : {200, 300, 500, 750, 800, 850, 900, 925, 950, 1000}) {
            for (auto name : {"temperature", "geopotential", "rel_humidity"}) {
                auto field = atlas::Field(name, atlas::array::DataType("real64"), make_shape(0));
                set_metadata(field.metadata(), level, step);
                auto single_test_field = create_single_test_field(field.metadata(), sz);
                gl_fields.push_back(single_test_field);
                auto atlas_field = create_atlas_field(single_test_field);
                atlas_field.metadata() = field.metadata();
                global_fields.push_back(std::move(atlas_field));
            }
        }
    }

    return gl_fields;
}

auto scatter_atlas_field(const TestField& gl_field) -> atlas::Field {
    if (transport.client()) {
        auto idxmap =
            create_local_to_global(field_size(), transport.no_clients(), transport.client_rank());
        return create_local_field(gl_field, idxmap);
    } else {
        return atlas::Field("dummy", atlas::array::DataType("real64"), make_shape(field_size()));
    }
}

}  // namespace

CASE("Test that fields ") {
    // Set up global data
    field_size() = 1999;
    auto gl_field = create_global_test_data(field_size());

    // Create fields from global data
    auto fields = std::vector<atlas::Field>{};
    for (const auto& glfl : gl_field) {
        auto field = scatter_atlas_field(glfl);
        field.metadata() = unpack_metadata(glfl.first);
        fields.push_back(std::move(field));
    }

    SECTION("are distributed and dispatched to correct plan") {
        if (transport.client()) {
            Distributor distributor{transport};
            for (auto field : fields) {
                distributor.sendField(field);
            }

            distributor.sendForecastComplete();
        } else {
            EXPECT(transport.server());

            Dispatcher dispatcher{transport};

            // Receive field
            dispatcher.listen();
        }

        transport.synchronise();
        if (transport.global_rank() == root()) {
            for (const auto& field : global_fields) {
                const auto& metadata = field.metadata();
                eckit::PathName file{metadata.get<std::string>("name") +
                                     "::" + std::to_string(metadata.get<int>("levels")) +
                                     "::" + std::to_string(metadata.get<int>("steps"))};

                auto actual = file_content(file);
                auto expected = pack_atlas_field(field);
                file.unlink();
                EXPECT(actual == expected);
            }
        }
    }
}

//-----------------------------------------------------------------------------

}  // namespace test
}  // namespace server
}  // namespace multio

int main(int argc, char** argv) {
    return run_tests(argc, argv);
}
