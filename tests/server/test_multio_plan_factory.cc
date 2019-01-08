
#include "TestServerHelpers.h"
#include "create_random_data.h"

#include "eckit/filesystem/PathName.h"
#include "eckit/testing/Test.h"

#include "atlas/array.h"
#include "atlas/field/Field.h"

#include "multio/server/LocalPlan.h"
#include "multio/server/Message.h"
#include "multio/server/Plan.h"
#include "multio/server/PlanFactory.h"
#include "multio/server/SerialisationHelpers.h"

using multio::test::file_content;

namespace multio {
namespace server {
namespace test {

CASE("Use plan factory to create plan") {
    field_size() = 8;
    auto maps = std::vector<std::vector<int>>{{1, 2, 5, 7}, {0, 3, 4, 6}};

    auto planFactory = PlanFactory{maps.size()};

    for (auto ii = 0u; ii != maps.size();) {
        auto test_plan = LocalPlan{"atm_grid", maps[ii]};
        test_plan.metadata.set("aggregation", "indexed");
        Message msg(0, ii, msg_tag::plan_data);
        local_plan_to_message(test_plan, msg);
        msg.rewind();
        EXPECT((++ii < maps.size()) ? !planFactory.tryCreate(msg) : planFactory.tryCreate(msg));
    }

    auto plan = planFactory.handOver("atm_grid");

    SECTION("Carry out plan") {
        // Create global field to test against
        auto test_field = set_up_atlas_test_field("temperature");
        std::vector<int> glbidx(field_size());
        std::iota(begin(glbidx), end(glbidx), 0);

        // Create local messages
        auto ii = 0;
        for (auto&& map : maps) {
            auto field = create_local_field(test_field, std::move(map));

            Message msg(0, ii++, msg_tag::field_data);
            atlas_field_to_message(field, msg);

            msg.rewind();
            plan.process(msg);
        }

        multio::test::TestFile file{"temperature::850::1"};
        auto actual = file_content(file.name());
        auto expected = pack_atlas_field(test_field);
        EXPECT(actual == expected);
    }
}

}  // namespace server
}  // namespace test
}  // namespace multio

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}
