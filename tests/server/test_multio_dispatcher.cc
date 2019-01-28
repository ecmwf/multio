
#include "eckit/testing/Test.h"

#include "multio/server/Dispatcher.h"
#include "multio/server/Distributor.h"
#include "multio/server/Message.h"
#include "multio/server/MpiTransport.h"
#include "multio/server/SerialisationHelpers.h"

#include "TestServerHelpers.h"

using namespace eckit::testing;
using atlas::array::make_shape;
using multio::test::file_content;

namespace multio {
namespace server {
namespace test {

namespace {
const auto nServers = 1u;
const Transport& transport = MpiTransport{"dispatcher test", nServers};

auto scatter_atlas_field(const atlas::Field& gl_field) -> atlas::Field {
    if (transport.client()) {
        auto idxmap =
            create_partial_mapping(field_size(), transport.noClients(), transport.clientRank());
        return create_local_field(gl_field, idxmap);
    } else {
        return atlas::Field("dummy", atlas::array::DataType("real64"), make_shape(field_size()));
    }
}
}  // namespace

CASE("Test that distributor-dispatcher pair ") {

    SECTION(" is created correctly") {
        if(transport.client()) {
            Distributor dist{transport};
            std::ostringstream oss;
            oss << dist;
            EXPECT(oss.str() == "Distributor initialised with MpiTransport[dispatcher test]");
        } else {
            EXPECT(transport.server());

            Dispatcher dispatcher{transport};
            std::ostringstream oss;
            oss << dispatcher;
            EXPECT(oss.str() == "Dispatcher initialised with MpiTransport[dispatcher test]");
        }
    }

    SECTION(" communicates correctly") {
        ASSERT(transport.size() == 3);
        field_size() = 8;

        // Create test field
        auto test_field = set_up_atlas_test_field("temperature");

        // Create local atlas field
        auto field = scatter_atlas_field(test_field);

        if (transport.client()) {
            Distributor distributor{transport};

            // Send field
            distributor.sendPartialField(field);

            // Notify server there is nothing more to send
            distributor.sendNotification(msg_tag::forecast_complete);
        } else {
            EXPECT(transport.server());

            Dispatcher dispatcher{transport};

            // Receive field
            dispatcher.eventLoop();
        }

        transport.synchronise();
        if (transport.globalRank() == root()) {
            multio::test::TestFile file{"temperature::850::1"};
            auto actual = file_content(file.name());
            auto expected = pack_atlas_field(test_field);
            EXPECT(actual == expected);
        }
    }
}

//-----------------------------------------------------------------------------

}  // namespace test
}  // namespace server
}  // namespace multio

int main(int argc, char** argv) {
    return run_tests(argc, argv);
}
