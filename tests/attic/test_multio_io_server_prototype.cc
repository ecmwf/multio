
#include "eckit/testing/Test.h"

#include "atlas/array.h"
#include "atlas/field/Field.h"
#include "atlas/util/Metadata.h"

#include "multio/attic/Dispatcher.h"
#include "multio/attic/Distributor.h"
#include "multio/attic/PartialMapping.h"
#include "multio/attic/MpiTransport.h"
#include "multio/attic/SerialisationHelpers.h"

#include "TestServerHelpers.h"

using namespace eckit::testing;
using atlas::array::make_shape;
using multio::test::file_content;

namespace multio {
namespace attic {
namespace test {

namespace {

const Transport& transport() {
    static const Transport& transport = MpiTransport{"i/o attic test", 3};
    return transport;
}

// const auto steps = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16};
// const auto levels = {200, 300, 500, 750, 800, 850, 900, 925, 950, 1000};
const auto steps = {0, 1, 2, 3};
const auto levels = {200, 300, 500};
const auto parameters = {"temperature", "geopotential", "rel_humidity"};

auto create_global_test_data(const size_t sz) -> std::vector<atlas::Field> {
    std::vector<atlas::Field> gl_fields;

    for (auto step : steps) {
        for (auto level : levels) {
            for (auto name : parameters) {
                atlas::util::Metadata metadata;
                set_metadata(metadata, name, level, step);
                gl_fields.push_back(create_global_test_field(metadata, sz));
            }
        }
        atlas::util::Metadata metadata;
        set_metadata(metadata, "sst", 0, step);
        gl_fields.push_back(create_global_test_field(metadata, sz));
    }

    return gl_fields;
}

auto scatter_atlas_field(const atlas::Field& gl_field) -> atlas::Field {
    if (transport().client()) {
        auto idxmap =
            create_partial_mapping(field_size(), transport().noClients(), transport().clientRank());
        return create_local_field(gl_field, idxmap);
    } else {
        return atlas::Field("dummy", atlas::array::DataType("real64"), make_shape(field_size()));
    }
}

auto create_partial_fields(const std::vector<atlas::Field>& global_fields)
    -> std::vector<atlas::Field> {
    auto partial_fields = std::vector<atlas::Field>{};
    for (const auto& glfl : global_fields) {
        partial_fields.push_back(scatter_atlas_field(glfl));
    }

    return partial_fields;
}

}  // namespace

CASE("Test that fields ") {
    // Set up global data
    field_size() = 19;

    auto global_fields = create_global_test_data(field_size());
    auto partial_fields = create_partial_fields(global_fields);

    SECTION("are distributed and dispatched to correct plan") {
        if (transport().client()) {
            Distributor distributor{transport()};
            for (auto ii = 0u; ii != partial_fields.size(); ++ii) {
                distributor.sendPartialField(partial_fields[ii]);
                if (partial_fields.size() % steps.size() == steps.size() - 1) {
                    distributor.sendNotification(msg_tag::step_complete);
                }
            }

            distributor.sendNotification(msg_tag::forecast_complete);
        } else {
            EXPECT(transport().server());

            Dispatcher dispatcher{transport()};

            // Receive field
            dispatcher.eventLoop();
        }

        transport().synchronise();
        if (transport().globalRank() == root()) {
            std::cout << "--- We are root... Start testing file contents..." << std::endl;
            for (const auto& field : global_fields) {
                const auto& metadata = field.metadata();
                multio::test::TestFile file{metadata.get<std::string>("name") +
                                     "::" + std::to_string(metadata.get<int>("level")) +
                                     "::" + std::to_string(metadata.get<int>("step"))};

                auto actual = file_content(file.name());
                auto expected = pack_atlas_field(field);

                EXPECT(actual == expected);
            }
        }
    }
}

//-----------------------------------------------------------------------------

}  // namespace test
}  // namespace attic
}  // namespace multio

int main(int argc, char** argv) {
    return run_tests(argc, argv);
}
