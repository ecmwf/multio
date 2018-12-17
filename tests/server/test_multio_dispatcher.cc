
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

auto scatter_atlas_field(const TestField& gl_field) -> atlas::Field {
    if (transport.client()) {
        auto idxmap =
            create_local_to_global(field_size(), transport.noClients(), transport.clientRank());
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
        auto global_atlas_field =
            atlas::Field("temperature", atlas::array::DataType("real64"), make_shape(0));
        set_metadata(global_atlas_field.metadata(), 850, 1);
        auto test_field = create_single_test_field(global_atlas_field.metadata(), field_size());

        // Create global atlas field
        global_atlas_field = create_atlas_field(test_field);
        global_atlas_field.metadata() = unpack_metadata(test_field.first);

        // Create local atlas field
        auto field = scatter_atlas_field(test_field);
        field.metadata() = unpack_metadata(test_field.first);

        if (transport.client()) {
            Distributor distributor{transport};

            // Send field
            distributor.sendField(field);

            // Notify server there is nothing more to send
            distributor.sendForecastComplete();
        } else {
            EXPECT(transport.server());

            Dispatcher dispatcher{transport};

            // Receive field
            dispatcher.listen();
        }

        transport.synchronise();
        if (transport.globalRank() == root()) {
            eckit::PathName file{"temperature::850::1"};
            auto actual = file_content(file);
            auto expected = pack_atlas_field(global_atlas_field);
            file.unlink();
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
