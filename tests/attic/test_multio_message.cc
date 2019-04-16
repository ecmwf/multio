
#include <cstring>

#include "TestServerHelpers.h"

#include "multio/attic/PartialMapping.h"
#include "multio/attic/Message.h"
#include "multio/attic/print_buffer.h"
#include "multio/attic/SerialisationHelpers.h"

#include "atlas/field/Field.h"

#include "eckit/mpi/Comm.h"
#include "eckit/testing/Test.h"

using namespace eckit::testing;

namespace multio {
namespace attic {
namespace test {

using eckit::mpi::comm;

CASE("Test message returns the same plan as string") {

    auto test_map = PartialMapping{"test_field", {7, 23, 43, 91}};

    auto dest = pack_mapping(test_map);

    Message msg(0, -1, msg_tag::message_data);
    mapping_to_message(test_map, msg);

    EXPECT(dest.size() == msg.size());
    EXPECT(std::memcmp(&dest[0], msg.data(), msg.size()) == 0);
}

CASE("Test message returns the same field as string") {

    // Set up data
    auto field = set_up_atlas_test_field("temperature");

    auto dest = pack_atlas_field(field);

    Message msg(0, -1, msg_tag::field_data);
    atlas_field_to_message(field, msg);

    EXPECT(dest.size() == msg.size());
    EXPECT(std::memcmp(&dest[0], msg.data(), msg.size()) == 0);
}

CASE("Test identity after writing and reading plan") {
    ASSERT(comm().size() == 2);

    auto test_map = PartialMapping{"test_field", {7, 23, 43, 91}};

    auto source = 0;
    auto dest = 1;
    if (comm().rank() == static_cast<size_t>(source)) {

        Message msg(0, dest, msg_tag::message_data);
        mapping_to_message(test_map, msg);

        comm().send<void>(msg.data(), msg.size(), msg.peer(), msg.tag());

        EXPECT(msg.tag() == msg_tag::message_data);

    } else {
        EXPECT(comm().rank() == static_cast<size_t>(dest));
        auto status = comm().probe(comm().anySource(), comm().anyTag());

        Message msg(comm().getCount<void>(status), status.source(), status.tag());
        comm().receive<void>(msg.data(), msg.size(), msg.peer(), msg.tag());

        auto received_map = unpack_mapping(msg);
        EXPECT(test_map == received_map);
    }
}

CASE("Test identity after writing and reading atlas field") {
    ASSERT(comm().size() == 2);

    // Set up data
    auto test_field = set_up_atlas_test_field("temperature");

    auto source = 0;
    auto dest = 1;
    if (comm().rank() == static_cast<size_t>(source)) {

        Message msg(0, dest);
        atlas_field_to_message(test_field, msg);

        comm().send<void>(msg.data(), msg.size(), msg.peer(), msg.tag());

        EXPECT(msg.tag() == msg_tag::field_data);
        EXPECT(msg.size() == 307u);
    } else {
        EXPECT(comm().rank() == static_cast<size_t>(dest));
        auto status = comm().probe(comm().anySource(), comm().anyTag());

        Message msg(comm().getCount<void>(status), status.source(), status.tag());
        comm().receive<void>(msg.data(), msg.size(), msg.peer(), msg.tag());

        auto received_field = unpack_atlas_field(msg);

        std::ostringstream expected;
        print_atlas_info(test_field, expected);

        std::ostringstream actual;
        print_atlas_info(received_field, actual);

        EXPECT(expected.str() == actual.str());
    }
}

//-----------------------------------------------------------------------------

}  // namespace test
}  // namespace attic
}  // namespace multio

int main(int argc, char** argv) {
    return run_tests(argc, argv);
}
