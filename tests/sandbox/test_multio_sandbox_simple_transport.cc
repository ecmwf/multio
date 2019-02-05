
#include <memory>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/testing/Test.h"

#include "multio/sandbox/SimpleTransport.h"

using namespace eckit::testing;

namespace multio {
namespace sandbox {
namespace test {

CASE("Test simple transport layer") {
    eckit::LocalConfiguration config;
    config.set("name", "simple");
    std::unique_ptr<sandbox::Transport> transport{new sandbox::SimpleTransport{config}};

    std::ostringstream oss;
    oss << *transport;

    EXPECT(oss.str() == "Transport[simple]");

    { // Client

        auto test_data = std::string("{7, 23, 43, 91}");
        auto msg = Message{0, 7, MsgTag::mapping_data};
        msg.write(test_data.data(), test_data.size());
        transport->send(msg);

        test_data = std::string("{19, 49, 67, 89}");
        msg = Message{0, 11, MsgTag::mapping_data};
        msg.write(test_data.data(), test_data.size());
        transport->send(msg);

        test_data = std::string("{11, 17, 29, 41}");
        msg = Message{0, 13, MsgTag::mapping_data};
        msg.write(test_data.data(), test_data.size());
        transport->send(msg);

    }

    { // Server
        std::ostringstream oss;
        Message msg{};
        transport->receive(msg);
        EXPECT(msg.peer() == 7);
        oss << msg;
        auto vals = oss.str().substr(oss.str().size() - msg.size() - 1, msg.size());
        EXPECT(vals == "{7, 23, 43, 91}");

        msg = Message{};
        transport->receive(msg);
        EXPECT(msg.peer() == 11);
        oss << msg;
        vals = oss.str().substr(oss.str().size() - msg.size() - 1, msg.size());
        EXPECT(vals == "{19, 49, 67, 89}");

        msg = Message{};
        transport->receive(msg);
        EXPECT(msg.peer() == 13);
        oss << msg;
        vals = oss.str().substr(oss.str().size() - msg.size() - 1, msg.size());
        EXPECT(vals == "{11, 17, 29, 41}");
    }
}

}  // namespace sandbox
}  // namespace test
}  // namespace multio

int main(int argc, char** argv) {
    return run_tests(argc, argv);
}
