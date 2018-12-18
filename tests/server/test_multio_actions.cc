
#include "eckit/testing/Test.h"
#include "eckit/filesystem/TmpFile.h"

#include "atlas/array.h"

#include "multio/server/Action.h"
#include "multio/server/Aggregation.h"
#include "multio/server/print_buffer.h"
#include "multio/server/SerialisationHelpers.h"
#include "multio/server/Sink.h"

#include "create_random_data.h"
#include "TestServerHelpers.h"

using multio::test::file_content;
using multio::test::make_configured_file_sink;

namespace multio {
namespace server {
namespace test {

CASE("Test that aggregation action constructs successfully") {
    auto maps = std::vector<std::vector<int>>{{1, 2, 5, 7}, {0, 3, 4, 6}};
    std::unique_ptr<Action> action(new Aggregation{std::move(maps)});
}

CASE("Test that sink action constructs successfully") {
    eckit::PathName file_path = eckit::TmpFile();
    std::unique_ptr<Action> action(new Sink{make_configured_file_sink(file_path)});

    file_path.unlink();
}

CASE("Test that aggregation action executes correctly") {
    auto maps = std::vector<std::vector<int>>{{1, 2, 5, 7}, {0, 3, 4, 6}};
    std::unique_ptr<Action> action{new Aggregation{maps}};

    field_size() = 8;
    auto test_field = set_up_atlas_test_field("temperature");

    auto atlas_field = atlas::Field{};
    auto ii = 0;
    do {
        atlas_field = create_local_field(test_field, maps[ii]);
        action->execute(atlas_field, ii++);
    } while (not action->complete(atlas_field));

    auto view = atlas::array::make_view<double, 1>(atlas_field);
    auto actual = std::vector<double>{};
    copy_n(view.data(), view.size(), back_inserter(actual));

    view = atlas::array::make_view<double, 1>(atlas_field);
    auto expected = std::vector<double>{};
    copy_n(view.data(), view.size(), back_inserter(expected));

    EXPECT(actual == expected);
}

CASE("Test that sink action executes correctly") {
    std::unique_ptr<Action> action{new Sink{nullptr}};

    field_size() = 29;
    auto test_field = set_up_atlas_test_field("temperature");
    std::vector<int> local_to_global(field_size());
    std::iota(begin(local_to_global), end(local_to_global), 0);

    auto atlas_field = create_local_field(test_field, local_to_global);

    action->execute(atlas_field);

    eckit::PathName file_path{"temperature::850::1"};
    auto actual = file_content(file_path);
    auto expected = pack_atlas_field(atlas_field);
    EXPECT(actual == expected);

    file_path.unlink();
}

CASE("Test that aggrigation and sink actions together execute correctly") {
    auto maps = std::vector<std::vector<int>>{{1, 2, 5, 7}, {0, 3, 4, 6}};
    std::unique_ptr<Action> action(new Aggregation{maps});

    field_size() = 8;
    auto test_field = set_up_atlas_test_field("temperature");

    auto atlas_field = atlas::Field{};
    auto ii = 0;
    do {
        atlas_field = create_local_field(test_field, maps[ii]);
        action->execute(atlas_field, ii++);
    } while (not action->complete(atlas_field));

    eckit::PathName file_path{"temperature::850::1"};
    action.reset(new Sink{make_configured_file_sink(file_path)});
    action->execute(atlas_field);

    auto actual = file_content(file_path);
    auto expected = pack_atlas_field(atlas_field);
    EXPECT(actual == expected);

    file_path.unlink();
}

}  // namespace server
}  // namespace test
}  // namespace multio

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}
