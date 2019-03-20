
#include "eckit/testing/Test.h"

#include "atlas/array.h"

#include "multio/server/Aggregation.h"
#include "multio/server/Message.h"
#include "multio/server/Mappings.h"
#include "multio/server/PartialMapping.h"
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

namespace {
void registerMap(const std::vector<std::vector<int>>& maps) {
    for (auto ii = 0u; ii != maps.size(); ++ii) {
        auto test_map = PartialMapping{"scattered", maps[ii]};
        test_map.metadata.set("map_count", maps.size());
        Message msg(0, ii, msg_tag::message_data);
        mapping_to_message(test_map, msg);
        msg.rewind();
        Mappings::instance().add(msg);
    }
}
}  // namespace

CASE("Test that plan with three actions ") {
    field_size() = 8;

    auto maps = std::vector<std::vector<int>>{{1, 2, 5, 7}, {0, 3, 4, 6}};
    auto field_name = eckit::TmpFile().baseName() + "::temperature";
    multio::test::TestFile file{field_name + "::850::1"};

    std::unique_ptr<Action> root{new Select{{"prognostic"}}};
    auto it = root.get();
    it = it->add(std::unique_ptr<Action>{new Aggregation{}});
    it->add(std::unique_ptr<Action>{new Sink{make_configured_file_sink(file.name())}});

    SECTION("constructs correctly") {
        Plan plan{"test_map", std::move(root)};
    }

    SECTION("processes message correctly") {
        Plan plan{"test_map", std::move(root)};

        registerMap(maps);

        // Create global field to test against
        auto test_field = set_up_atlas_test_field(field_name);

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
        auto test_field = set_up_atlas_test_field(field_name);

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
