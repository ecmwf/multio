
#include "TestServerHelpers.h"
#include "create_random_data.h"

#include "eckit/filesystem/PathName.h"
#include "eckit/testing/Test.h"

#include "atlas/array.h"
#include "atlas/field/Field.h"

#include "multio/server/PartialMapping.h"
#include "multio/server/Message.h"
#include "multio/server/Plan.h"
#include "multio/server/PlanAssembler.h"
#include "multio/server/SerialisationHelpers.h"

using multio::test::file_content;

namespace multio {
namespace server {
namespace test {

CASE("Test that plan assembler creates plan correctly and ") {
    field_size() = 8;
    auto maps = std::vector<std::vector<int>>{{1, 2, 5, 7}, {0, 3, 4, 6}};

    PlanAssembler planAssembler;

    for (auto ii = 0u; ii != maps.size();) {
        auto test_map = PartialMapping{"atm_grid", maps[ii]};
        test_map.metadata.set("no_maps", maps.size());
        Message msg(0, ii, msg_tag::plan_data);
        mapping_to_message(test_map, msg);
        msg.rewind();
        EXPECT((++ii < maps.size()) ? !planAssembler.tryCreate(msg) : planAssembler.tryCreate(msg));
    }

    auto plan = planAssembler.handOver("atm_grid");

    SECTION("carries out plan when corresponding message is passed") {
        // Create global field to test against
        auto test_field = set_up_atlas_test_field("temperature");

        // Create local messages
        auto ii = 0;
        for (auto&& map : maps) {
            auto field = create_local_field(test_field, std::move(map));

            auto msg = std::make_shared<Message>(0, ii++, msg_tag::field_data);
            atlas_field_to_message(field, *msg);

            msg->rewind();
            plan.process(msg);
        }

        multio::test::TestFile file{"temperature::850::1"};

        auto actual = file_content(file.name());
        auto expected = pack_atlas_field(test_field);
        EXPECT(actual == expected);
    }

    SECTION("returns early when non-matching message is passed") {
        // Create global field to test against
        auto test_field = set_up_atlas_test_field("temperature");
        test_field.metadata().set("plan_name", "dummy");

        // Create local messages
        auto ii = 0;
        for (auto&& map : maps) {
            auto field = create_local_field(test_field, std::move(map));

            auto msg = std::make_shared<Message>(0, ii++, msg_tag::field_data);
            atlas_field_to_message(field, *msg);

            msg->rewind();
            plan.process(msg);
        }

        multio::test::TestFile file{"temperature::850::1"};
        EXPECT(file_content(file.name()).empty());
    }
}

}  // namespace server
}  // namespace test
}  // namespace multio

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}
