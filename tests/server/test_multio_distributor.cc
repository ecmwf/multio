
#include "eckit/testing/Test.h"

#include "atlas/array.h"

#include "multio/Aggregator.h"
#include "multio/Distributor.h"
#include "multio/MpiTransport.h"
#include "multio/make_unique.h"

#include "TestServerHelpers.h"

using namespace eckit::testing;

namespace multio {
namespace server {
namespace test {

namespace {
const auto nServers = 1u;
const Transport& transport = MpiTransport{"distributor test", nServers};
}  // namespace

CASE("Test that distributor-aggregator pair ") {

    SECTION(" is created correctly") {
        if(transport->client()) {
            Distributor dist{*transport};
            std::ostringstream oss;
            oss << dist;
            EXPECT(oss.str() == "Distributor initialised with MpiTransport[distributor test]");
        } else {
            EXPECT(transport->server());

            Aggregator aggregator{transport};
            std::ostringstream oss;
            oss << aggregator;
            EXPECT(oss.str() == "Aggregator initialised with MpiTransport[distributor test]");
        }
    }

    SECTION(" communicates correctly") {
        ASSERT(transport->size() == 2);

        auto test_field = set_up_atlas_test_field("temperature");

        if (transport->client()) {
            Distributor distributor{*transport};

            // Send field
            distributor.sendPartialField(test_field);

            // Notify server there is nothing more to send
            distributor.sendForecastComplete();
        } else {
            EXPECT(transport->server());

            Aggregator aggregator{transport};

            // Receive field
            aggregator.listen();

            EXPECT(aggregator.field_exists(test_field));

            auto actual = aggregator.get_field(test_field);

            auto view = atlas::array::make_view<double, 1>(test_field);
            auto expected = std::vector<double>{};
            copy_n(view.data(), view.size(), back_inserter(expected));

            EXPECT(expected == actual);
        }
    }
}

//-----------------------------------------------------------------------------

}  // namespace server
}  // namespace test
}  // namespace multio

int main(int argc, char** argv) {
    return run_tests(argc, argv);
}
