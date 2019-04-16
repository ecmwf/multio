
#include "TestServerHelpers.h"
#include "create_random_data.h"

#include "eckit/filesystem/PathName.h"
#include "eckit/testing/Test.h"

#include "atlas/array.h"
#include "atlas/field/Field.h"

#include "multio/attic/PartialMapping.h"
#include "multio/attic/Mappings.h"
#include "multio/attic/Message.h"
#include "multio/attic/Plan.h"
#include "multio/attic/PlanAssembler.h"
#include "multio/attic/SerialisationHelpers.h"

using multio::test::file_content;

namespace multio {
namespace attic {
namespace test {

CASE("Test that when using singleton plan is created correctly") {
    field_size() = 8;
    auto maps = std::vector<std::vector<int>>{{1, 2, 5, 7}, {0, 3, 4, 6}};

    for (auto ii = 0u; ii != maps.size(); ++ii) {
        auto test_map = PartialMapping{"scattered", maps[ii]};
        test_map.metadata.set("map_count", maps.size());
        Message msg(0, ii, msg_tag::message_data);
        mapping_to_message(test_map, msg);
        msg.rewind();
        Mappings::instance().add(msg);
    }

    auto ii = 0u;
    for (const auto& indices : Mappings::instance().get("scattered")) {
        EXPECT(indices == maps[ii++]);
    }

    PlanAssembler planAssembler;
    auto all_plans = planAssembler.createAllPlans();

    auto field_name = eckit::TmpFile().baseName() + "::temperature";
    auto test_field = set_up_atlas_test_field(field_name);

    // Create local messages
    ii = 0;
    for (auto&& map : maps) {
        auto field = create_local_field(test_field, std::move(map));

        auto msg = std::make_shared<Message>(0, ii++, msg_tag::field_data);
        atlas_field_to_message(field, *msg);

        msg->rewind();
        for (const auto& plan : all_plans) {
            plan.process(msg);
        }
    }

    multio::test::TestFile file{field_name + "::850::1"};

    auto actual = file_content(file.name());
    auto expected = pack_atlas_field(test_field);
    EXPECT(actual == expected);
}

}  // namespace attic
}  // namespace test
}  // namespace multio

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}
