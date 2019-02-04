
#include "eckit/config/LocalConfiguration.h"
#include "eckit/testing/Test.h"

#include "multio/sandbox/Message.h"
#include "multio/sandbox/ThreadTransport.h"
#include "multio/sandbox/Listener.h"

#include "../TestHelpers.h"

using namespace eckit::testing;

namespace multio {
namespace sandbox {
namespace test {

CASE("Test dummy transport layer") {
    eckit::LocalConfiguration config;
    config.set("name", "dummy");
    std::unique_ptr<sandbox::Transport> transport{new sandbox::ThreadTransport{config}};

    std::ostringstream oss;
    oss << *transport;

    EXPECT(oss.str() == "ThreadTransport[dummy]");

    { // Client
        // Open connections

        auto msg = Message{0, 7, MsgTag::open};
        transport->send(msg);
        msg = Message{0, 11, MsgTag::open};
        transport->send(msg);
        msg = Message{0, 13, MsgTag::open};
        transport->send(msg);

        auto test_data = std::string("{7, 23, 43, 91}");
        msg = Message{0, 7, MsgTag::mapping_data};
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

        // Close connections

        msg = Message{0, 7, MsgTag::close};
        transport->send(msg);
        msg = Message{0, 11, MsgTag::close};
        transport->send(msg);
        msg = Message{0, 13, MsgTag::close};
        transport->send(msg);
    }

    { // Server
        sandbox::Listener listener(*transport);

        listener.listen();

        multio::test::TestFile file{"test_output"};
        auto actual = multio::test::file_content(file.name());
        std::string expected = R"raw(Message(tag = 0, peer = 7, position = 15, buffer = {7, 23, 43, 91})
Message(tag = 0, peer = 11, position = 16, buffer = {19, 49, 67, 89})
Message(tag = 0, peer = 13, position = 16, buffer = {11, 17, 29, 41})
)raw";

        EXPECT(actual == expected);
    }
}

}  // namespace sandbox
}  // namespace test
}  // namespace multio

int main(int argc, char** argv) {
    return run_tests(argc, argv);
}
