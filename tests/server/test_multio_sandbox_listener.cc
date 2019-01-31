
#include "eckit/testing/Test.h"

#include "multio/server/Message.h"
#include "multio/server/PartialMapping.h"
#include "multio/server/SerialisationHelpers.h"

#include "multio/server/sandbox/DummyTransport.h"
#include "multio/server/sandbox/Listener.h"

#include "TestServerHelpers.h"

using namespace eckit::testing;

namespace multio {
namespace server {
namespace test {

CASE("Test dummy transport layer") {
    auto config = std::string{"dummy"};
    std::unique_ptr<sandbox::Transport> transport{new sandbox::DummyTransport{config}};

    std::ostringstream oss;
    oss << *transport;

    EXPECT(oss.str() == "DummyTransport[dummy]");

    { // Client
        // Open connections

        auto msg = Message{0, 7, msg_tag::open};
        transport->send(msg);
        msg = Message{0, 11, msg_tag::open};
        transport->send(msg);
        msg = Message{0, 13, msg_tag::open};
        transport->send(msg);

        auto test_data = std::string("{7, 23, 43, 91}");
        msg = Message{0, 7, msg_tag::message_data};
        msg.write(test_data.data(), test_data.size());
        transport->send(msg);
        msg.rewind();

        test_data = std::string("{19, 49, 67, 89}");
        msg = Message{0, 11, msg_tag::message_data};
        msg.write(test_data.data(), test_data.size());
        transport->send(msg);
        msg.rewind();

        test_data = std::string("{11, 17, 29, 41}");
        msg = Message{0, 13, msg_tag::message_data};
        msg.write(test_data.data(), test_data.size());
        transport->send(msg);

        // Close connections

        msg = Message{0, 7, msg_tag::close};
        transport->send(msg);
        msg = Message{0, 11, msg_tag::close};
        transport->send(msg);
        msg = Message{0, 13, msg_tag::close};
        transport->send(msg);
    }

    { // Server
        sandbox::Listener listener(*transport);

        listener.eventLoop();

        multio::test::TestFile file{"test_output"};
        auto actual = multio::test::file_content(file.name());
        std::string expected = R"raw(Message(tag = 0, peer = 7, position = 15, buffer = {7, 23, 43, 91})
Message(tag = 0, peer = 11, position = 16, buffer = {19, 49, 67, 89})
Message(tag = 0, peer = 13, position = 16, buffer = {11, 17, 29, 41})
)raw";

        EXPECT(actual == expected);
    }
}

}  // namespace server
}  // namespace test
}  // namespace multio

int main(int argc, char** argv) {
    return run_tests(argc, argv);
}
