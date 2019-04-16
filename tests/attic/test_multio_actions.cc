
#include "eckit/testing/Test.h"
#include "eckit/filesystem/TmpFile.h"

#include "atlas/array.h"

#include "multio/attic/Action.h"
#include "multio/attic/Aggregation.h"
#include "multio/attic/Mappings.h"
#include "multio/attic/PartialMapping.h"
#include "multio/attic/print_buffer.h"
#include "multio/attic/Select.h"
#include "multio/attic/SerialisationHelpers.h"
#include "multio/attic/Sink.h"

#include "create_random_data.h"
#include "TestServerHelpers.h"

using multio::test::file_content;
using multio::test::make_configured_file_sink;

namespace multio {
namespace attic {
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

CASE("Test that each individual action constructs successfully: ") {
    SECTION("Select action") { std::unique_ptr<Action> action(new Select{{"test_category"}}); }

    SECTION("Aggregation action") {
        auto maps = std::vector<std::vector<int>>{{1, 2, 5, 7}, {0, 3, 4, 6}};
        std::unique_ptr<Action> action(new Aggregation{});
    }

    SECTION("Sink action") {
        const eckit::PathName& file_path = eckit::TmpFile();
        std::unique_ptr<Action> action(new Sink{make_configured_file_sink(file_path)});
    }
}

CASE("Test that select action executes correctly") {
    std::unique_ptr<Action> action{new Select{{"test_category"}}};

    field_size() = 8;
    auto test_field = set_up_atlas_test_field("temperature");

    auto msg = std::make_shared<Message>(0, -1, msg_tag::field_data);
    atlas_field_to_message(test_field, *msg);

    msg->rewind();
    EXPECT(not action->execute(msg));

    test_field.metadata().set<std::string>("category", "test_category");
    msg->rewind();
    atlas_field_to_message(test_field, *msg);

    msg->rewind();
    EXPECT(action->execute(msg));
}

CASE("Test actions execute correctly: ") {
    auto maps = std::vector<std::vector<int>>{{1, 2, 5, 7}, {0, 3, 4, 6}};

    SECTION("Aggregation action") {
        registerMap(maps); // Do only once if we are going to re-use it
        std::unique_ptr<Action> action{new Aggregation{}};

        field_size() = 8;
        auto test_field = set_up_atlas_test_field("temperature");

        auto msgs = std::vector<std::shared_ptr<Message>>(maps.size());
        auto ii = 0;
        do {
            msgs[ii] = std::make_shared<Message>(0, ii, msg_tag::field_data);
            auto atlas_field = create_local_field(test_field, maps[ii]);
            atlas_field_to_message(atlas_field, *msgs[ii]);
            msgs[ii]->rewind();
        } while (not action->execute(msgs[ii++]));

        auto atlas_field = unpack_atlas_field(*msgs[--ii]);
        auto view = atlas::array::make_view<double, 1>(atlas_field);
        auto actual = std::vector<double>{};
        copy_n(view.data(), view.size(), back_inserter(actual));

        view = atlas::array::make_view<double, 1>(test_field);
        auto expected = std::vector<double>{};
        copy_n(view.data(), view.size(), back_inserter(expected));

        EXPECT(actual == expected);
    }

    SECTION("Sink action") {
        std::unique_ptr<Action> action{new Sink{nullptr}};

        field_size() = 29;
        auto field_name = eckit::TmpFile().baseName() + "::temperature";
        auto test_field = set_up_atlas_test_field(field_name);
        std::vector<int> local_to_global(field_size());
        std::iota(begin(local_to_global), end(local_to_global), 0);

        auto atlas_field = create_local_field(test_field, local_to_global);

        auto msg = std::make_shared<Message>(0, -1, msg_tag::field_data);
        atlas_field_to_message(atlas_field, *msg);

        msg->rewind();
        action->execute(msg);

        multio::test::TestFile file{field_name + "::850::1"};
        auto actual = file_content(file.name());
        auto expected = pack_atlas_field(unpack_atlas_field(*msg));

        EXPECT(actual == expected);
    }

    SECTION("Aggregation action together with sink action") {
        auto maps = std::vector<std::vector<int>>{{1, 2, 5, 7}, {0, 3, 4, 6}};
        std::unique_ptr<Action> action(new Aggregation{});

        field_size() = 8;
        auto field_name = eckit::TmpFile().baseName() + "::temperature";
        auto test_field = set_up_atlas_test_field(field_name);

        auto msgs = std::vector<std::shared_ptr<Message>>(maps.size());
        auto ii = 0;
        do {
            msgs[ii] = std::make_shared<Message>(0, ii, msg_tag::field_data);
            auto atlas_field = create_local_field(test_field, maps[ii]);
            atlas_field_to_message(atlas_field, *msgs[ii]);
            msgs[ii]->rewind();
        } while (not action->execute(msgs[ii++]));

        multio::test::TestFile file{field_name + "::850::1"};
        action.reset(new Sink{make_configured_file_sink(file.name())});
        action->execute(msgs[--ii]);

        auto actual = file_content(file.name());
        auto expected = pack_atlas_field(unpack_atlas_field(*msgs[ii]));

        EXPECT(actual == expected);
}

}

}  // namespace attic
}  // namespace test
}  // namespace multio

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}
