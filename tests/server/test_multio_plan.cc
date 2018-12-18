
#include "eckit/testing/Test.h"

#include "atlas/array.h"

#include "multio/server/Aggregation.h"
#include "multio/server/Plan.h"
#include "multio/server/SerialisationHelpers.h"
#include "multio/server/Sink.h"
#include "multio/server/Message.h"

#include "create_random_data.h"
#include "TestServerHelpers.h"

using multio::test::file_content;
using multio::test::make_configured_file_sink;

namespace multio {
namespace server {
namespace test {

CASE("Test that plan with two actions ") {
    ActionList actions;
    field_size() = 8;
    auto maps = std::vector<std::vector<int>>{{1, 2, 5, 7}, {0, 3, 4, 6}};
    multio::test::TestFile file{"temperature::850::1"};

    actions.emplace_back(new Aggregation{maps});
    actions.emplace_back(new Sink{make_configured_file_sink(file.name())});

    SECTION("constructs correctly") {
        auto plan = Plan{"test_plan", std::move(actions)};
    }

    SECTION("processes message correctly") {
        auto plan = Plan{"test_plan", std::move(actions)};

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

        auto actual = file_content(file.name());
        auto expected = pack_atlas_field(test_field);
        EXPECT(actual == expected);
    }

    SECTION("executes actions independently") {

        // Create global field to test against
        auto test_field = set_up_atlas_test_field("temperature");

        std::vector<int> glbidx(field_size());
        std::iota(begin(glbidx), end(glbidx), 0);

        // Create local atlas fields
        auto atlas_fields = std::vector<atlas::Field>{};
        for (auto&& map : maps) {
            auto fld = create_local_field(test_field, std::move(map));
            atlas_fields.push_back(std::move(fld));
        }

        // Carry out plan
        auto ii = 0;
        for (auto& fld : atlas_fields) {
            ASSERT(static_cast<size_t>(ii) < atlas_fields.size());
            auto it = begin(actions);
            do {
                (*it)->execute(fld, ii++);
            } while ((*it)->complete(fld) && ++it != end(actions));
        }

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
