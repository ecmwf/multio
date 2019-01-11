
#include "eckit/testing/Test.h"

#include "atlas/array.h"
#include "atlas/field/Field.h"
#include "atlas/util/Metadata.h"

#include "multio/server/Dispatcher.h"
#include "multio/server/Distributor.h"
#include "multio/server/PartialMapping.h"
#include "multio/server/MpiTransport.h"
#include "multio/server/SerialisationHelpers.h"

#include "TestServerHelpers.h"

using namespace eckit::testing;
using atlas::array::make_shape;
using multio::test::file_content;

namespace multio {
namespace server {
namespace test {

namespace {

const auto nServers = 3u;
const Transport& transport = MpiTransport{"i/o server test", nServers};

auto create_global_test_data(const size_t sz) -> std::vector<atlas::Field> {
    std::vector<atlas::Field> gl_fields;

    for (auto step : {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16}) {
        for (auto level : {200, 300, 500, 750, 800, 850, 900, 925, 950, 1000}) {
            for (auto name : {"temperature", "geopotential", "rel_humidity"}) {
                atlas::util::Metadata metadata;
                set_metadata(metadata, name, level, step);
                gl_fields.push_back(create_global_test_field(metadata, sz));
            }
        }
    }

    return gl_fields;
}

auto scatter_atlas_field(const atlas::Field& gl_field) -> atlas::Field {
    if (transport.client()) {
        auto idxmap =
            create_partial_mapping(field_size(), transport.noClients(), transport.clientRank());
        return create_local_field(gl_field, idxmap);
    } else {
        return atlas::Field("dummy", atlas::array::DataType("real64"), make_shape(field_size()));
    }
}

}  // namespace

CASE("Test that fields ") {
    // Set up global data
    field_size() = 1999;

    auto global_fields = create_global_test_data(field_size());

    // Create local fields from global data
    auto fields = std::vector<atlas::Field>{};
    for (const auto& glfl : global_fields) {
        fields.push_back(scatter_atlas_field(glfl));
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
        if (transport.globalRank() == root()) {
            std::cout << "--- We are root... Start testing file contents..." << std::endl;
            for (const auto& field : global_fields) {
                const auto& metadata = field.metadata();
                multio::test::TestFile file{metadata.get<std::string>("name") +
                                     "::" + std::to_string(metadata.get<int>("levels")) +
                                     "::" + std::to_string(metadata.get<int>("steps"))};

                auto actual = file_content(file.name());
                auto expected = pack_atlas_field(field);

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
