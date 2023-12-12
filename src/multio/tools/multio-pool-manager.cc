
#include <unistd.h>
#include <sstream>
#include <thread>

#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"

#include "multio/LibMultio.h"
#include "multio/maestro/MaestroEvent.h"
#include "multio/maestro/MaestroSelector.h"
#include "multio/tools/MultioTool.h"


namespace multio {

namespace {

bool process_join_leave_events(MaestroSubscription& join_leave_subscription) {
    static uint16_t nbJoiners;
    static uint16_t nbLeavers;

    auto event = join_leave_subscription.poll();
    if (event) {
        auto tmp = event.raw_event();
        while (tmp) {
            switch (tmp->kind) {
                case MSTRO_POOL_EVENT_APP_JOIN:
                    ++nbJoiners;
                    LOG_DEBUG_LIB(LibMultio)
                        << " *** Application " << tmp->join.component_name << " is joining" << std::endl;
                    break;
                case MSTRO_POOL_EVENT_APP_LEAVE:
                    ++nbLeavers;
                    LOG_DEBUG_LIB(LibMultio) << " *** Application " << tmp->leave.appid << " is leaving" << std::endl;
                    break;
                default:
                    std::ostringstream os;
                    os << "Unexpected event " << tmp->kind;
                    throw eckit::SeriousBug{os.str()};
            }
            tmp = tmp->next;
        }
        join_leave_subscription.ack(event);
    }
    ASSERT(nbLeavers <= nbJoiners);
    return (nbJoiners == 0) || (nbLeavers < nbJoiners);
}

void track_components() {
    //    mstro_cdo_selector selector = nullptr;
    //    ASSERT(MSTRO_OK == mstro_cdo_selector_create(NULL, NULL, NULL, &selector));

    //    mstro_subscription subscription;
    //    ASSERT(MSTRO_OK == mstro_subscribe(selector,
    //                                       MSTRO_POOL_EVENT_APP_JOIN | MSTRO_POOL_EVENT_APP_LEAVE,
    //                                       MSTRO_SUBSCRIPTION_OPTS_DEFAULT, &subscription));

    MaestroSelector match_all_selector{nullptr};
    auto join_leave_subscription = match_all_selector.subscribe(MSTRO_POOL_EVENT_APP_JOIN | MSTRO_POOL_EVENT_APP_LEAVE,
                                                                MSTRO_SUBSCRIPTION_OPTS_DEFAULT);

    while (process_join_leave_events(join_leave_subscription))
        ;
}
}  // namespace

class PoolManager final : public MultioTool {
public:
    PoolManager(int argc, char** argv);
    ~PoolManager() = default;

private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << std::endl << "Usage: " << tool << " [options]" << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

    void finish(const eckit::option::CmdArgs&) override;

    void execute(const eckit::option::CmdArgs& args) override;

    int heartbeatInterval_;
};

PoolManager::PoolManager(int argc, char** argv) : multio::MultioTool(argc, argv) {}

void PoolManager::init(const eckit::option::CmdArgs& args) {
    args.get("heartbeat", heartbeatInterval_);

    ASSERT(MSTRO_OK == mstro_init(::getenv("MSTRO_WORKFLOW_NAME"), "Multio Pool Manager", 0));
}

void PoolManager::finish(const eckit::option::CmdArgs&) {
    auto s = mstro_pm_terminate();
    if (s != MSTRO_OK) {
        std::ostringstream oss;
        oss << "Failed to shut down pool: " << s << mstro_status_description(s) << std::endl;
        ASSERT_MSG(false, oss.str());
    }

    s = mstro_finalize();
    if (s != MSTRO_OK) {
        std::ostringstream oss;
        oss << "Failed to finalize: " << s << mstro_status_description(s) << std::endl;
        ASSERT_MSG(false, oss.str());
    }
}

void PoolManager::execute(const eckit::option::CmdArgs&) {
    auto s = mstro_pm_start();
    if (s != MSTRO_OK) {
        std::ostringstream oss;
        oss << "Failed to start pool manager: " << s << mstro_status_description(s) << std::endl;
        ASSERT_MSG(false, oss.str());
    }

    char* info = nullptr;
    s = mstro_pm_getinfo(&info);
    if (s != MSTRO_OK) {
        std::ostringstream oss;
        oss << "Failed to obtain pool contact info" << s << mstro_status_description(s) << std::endl;
    }

    eckit::Log::info() << MSTRO_ENV_POOL_INFO << ";" << info << ";" << std::endl;

    track_components();
}
}  // namespace multio

//---------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    multio::PoolManager tool(argc, argv);
    return tool.start();
}
