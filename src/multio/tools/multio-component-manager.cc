#include <sstream>

#include "eckit/log/Log.h"
#include "eckit/option/SimpleOption.h"

#include "multio/LibMultio.h"
#include "multio/maestro/MaestroCdo.h"
#include "multio/maestro/MaestroEvent.h"
#include "multio/maestro/MaestroSelector.h"
#include "multio/tools/MultioTool.h"

namespace multio {

class ComponentManager final : public MultioTool {
public:
    ComponentManager(int argc, char** argv);
    ~ComponentManager() = default;

private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << std::endl << "Usage: " << tool << " [options]" << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

    void finish(const eckit::option::CmdArgs&) override;

    void execute(const eckit::option::CmdArgs& args) override;

    int numberOfJoins_;

    MaestroCdo allReady_;
};

ComponentManager::ComponentManager(int argc, char** argv) : multio::MultioTool(argc, argv) {
    options_.push_back(new eckit::option::SimpleOption<uint64_t>("number-of-joins", "Wait for number-of-joins join events before sending an all-ready message"));
}

void ComponentManager::init(const eckit::option::CmdArgs& args) {
    args.get("number-of-joins", numberOfJoins_);

    ASSERT(MSTRO_OK == mstro_init(::getenv("MSTRO_WORKFLOW_NAME"), ::getenv("MSTRO_COMPONENT_NAME"), 0));
}

void ComponentManager::finish(const eckit::option::CmdArgs&) {
    auto s = mstro_finalize();
    if (s != MSTRO_OK) {
        std::ostringstream oss;
        oss << "Failed to finalize: " << s << mstro_status_description(s) << std::endl;
        ASSERT_MSG(false, oss.str());
    }
}

void ComponentManager::execute(const eckit::option::CmdArgs &) {
    uint16_t joinCount=0;
    uint16_t leaveCount=0;
    bool done = false;
    bool allReadySent = false;
    MaestroSelector selector{nullptr};
    auto subscription = selector.subscribe(
            MSTRO_POOL_EVENT_APP_JOIN|MSTRO_POOL_EVENT_APP_LEAVE,
            MSTRO_SUBSCRIPTION_OPTS_DEFAULT);

    while (not done) {
        auto event = subscription.poll();
        if (event) {
            auto tmp = event.raw_event();
            while (tmp) {
                switch(tmp->kind) {
                    case MSTRO_POOL_EVENT_APP_JOIN:
                        std::cout << mstro_clock() << ",JOIN," << tmp->serial
                                  << ", { " << tmp->join.appid << ",\"" << tmp->join.component_name << "\" }"
                                  << std::endl;
                        ++joinCount;
                        break;
                    case MSTRO_POOL_EVENT_APP_LEAVE:
                        std::cout << mstro_clock() << ",LEAVE," << tmp->serial
                                  << ", { " << tmp->leave.appid << " }"
                                  << std::endl;
                        ++leaveCount;
                        break;
                    default:
                        std::ostringstream os;
                        os << "Unexpected event " << tmp->kind;
                        throw eckit::SeriousBug{os.str()};
                }
                tmp = tmp->next;
            }
        }

    
        if (numberOfJoins_ > 0 and joinCount == numberOfJoins_ and not allReadySent) {
            allReady_ = MaestroCdo{"allClientsReady"};
            allReady_.seal();
            allReady_.offer();
            std::cout << mstro_clock() << ",ALL_READY,0, { 4294967295,\"SPECIAL MESSAGE\" }" << std::endl;
            allReadySent = true;
        }

        if ((0 < leaveCount) and (joinCount == leaveCount)) {
            allReady_.withdraw();
            allReady_.dispose();
            done = true;
        }
    }
}

}  // namespace multio

//---------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    multio::ComponentManager tool(argc, argv);
    return tool.start();
}
