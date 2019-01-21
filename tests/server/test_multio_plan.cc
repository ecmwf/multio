
#include "eckit/testing/Test.h"

#include "atlas/array.h"

#include "multio/server/Aggregation.h"
#include "multio/server/Message.h"
#include "multio/server/Plan.h"
#include "multio/server/SerialisationHelpers.h"
#include "multio/server/Select.h"
#include "multio/server/Sink.h"

#include "create_random_data.h"
#include "TestServerHelpers.h"

using multio::test::file_content;
using multio::test::make_configured_file_sink;

namespace multio {
namespace server {
namespace test {

CASE("Test that plan with three actions ") {
    field_size() = 8;

    auto maps = std::vector<std::vector<int>>{{1, 2, 5, 7}, {0, 3, 4, 6}};
    multio::test::TestFile file{"temperature::850::1"};

    std::unique_ptr<Action> root{new Select{"atm_grid"}};
    auto it = root.get();
    it = it->add(std::unique_ptr<Action>{new Aggregation{maps}});
    it->add(std::unique_ptr<Action>{new Sink{make_configured_file_sink(file.name())}});

    SECTION("constructs correctly") {
        Plan plan{"test_map", std::move(root)};
    }

    SECTION("processes message correctly") {
        Plan plan{"test_map", std::move(root)};

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

        auto actual = file_content(file.name());
        auto expected = pack_atlas_field(test_field);

        EXPECT(actual == expected);
    }

    SECTION("executes actions independently") {

        // Create global field to test against
        auto test_field = set_up_atlas_test_field("temperature");

        // Create local atlas fields
        auto atlas_fields = std::vector<atlas::Field>{};
        for (auto&& map : maps) {
            auto fld = create_local_field(test_field, std::move(map));
            atlas_fields.push_back(std::move(fld));
        }

        // Carry out plan
        auto msgs = std::vector<std::shared_ptr<Message>>(maps.size());
        auto ii = 0;
        do {
            msgs[ii] = std::make_shared<Message>(0, ii, msg_tag::field_data);
            auto atlas_field = create_local_field(test_field, maps[ii]);
            atlas_field_to_message(atlas_field, *msgs[ii]);
            msgs[ii]->rewind();
        } while (not root->execute(msgs[ii++]));

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
