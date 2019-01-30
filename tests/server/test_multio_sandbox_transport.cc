
#include "eckit/testing/Test.h"

#include "multio/server/Message.h"
#include "multio/server/PartialMapping.h"
#include "multio/server/SerialisationHelpers.h"

#include "multio/server/sandbox/DummyTransport.h"

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

    {
        auto test_data = std::string("{7, 23, 43, 91}");
        Message msg(0, 7, msg_tag::message_data);
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
    }

    {
        std::ostringstream oss;
        Message msg{};
        transport->receive(msg);
        EXPECT(msg.peer() == 7);
        oss << msg;
        auto vals = oss.str().substr(oss.str().size() - msg.size() - 2, msg.size());
        EXPECT(vals == "{7, 23, 43, 91}");

        msg = Message{};
        transport->receive(msg);
        EXPECT(msg.peer() == 11);
        oss << msg;
        vals = oss.str().substr(oss.str().size() - msg.size() - 2, msg.size());
        EXPECT(vals == "{19, 49, 67, 89}");

        msg = Message{};
        transport->receive(msg);
        EXPECT(msg.peer() == 13);
        oss << msg;
        vals = oss.str().substr(oss.str().size() - msg.size() - 2, msg.size());
        EXPECT(vals == "{11, 17, 29, 41}");
    }
}

}  // namespace server
}  // namespace test
}  // namespace multio

int main(int argc, char** argv) {
    return run_tests(argc, argv);
}
