
extern "C" {
#include <maestro.h>
}

#include <algorithm>

#include "eckit/log/Log.h"
#include "eckit/option/SimpleOption.h"
#include "eckit/log/Statistics.h"

#include "multio/tools/MultioTool.h"
#include "multio/util/ScopedTimer.h"

namespace multio {

class MaestroSyphon final : public multio::MultioTool {
public:  // methods
    MaestroSyphon(int argc, char** argv);

private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << std::endl << "Usage: " << tool << " [options]" << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

    void finish(const eckit::option::CmdArgs&) override;

    void execute(const eckit::option::CmdArgs& args) override;

    std::string reqFile_ = "";
    std::vector<mstro_cdo> requiredCdos_;
    unsigned cdoEventCount_ = 0;
    eckit::Timing timing_;
};

MaestroSyphon::MaestroSyphon(int argc, char** argv) : multio::MultioTool(argc, argv) {
    options_.push_back(
        new eckit::option::SimpleOption<std::string>("file", "File containing the requirements"));
}

void MaestroSyphon::init(const eckit::option::CmdArgs& args) {
    args.get("file", reqFile_);

    mstro_status s =
        mstro_init(::getenv("MSTRO_WORKFLOW_NAME"), ::getenv("MSTRO_COMPONENT_NAME"), 0);
    ASSERT(s == MSTRO_OK);
}

void MaestroSyphon::finish(const eckit::option::CmdArgs&) {
    ASSERT(mstro_finalize() == MSTRO_OK);
}

void MaestroSyphon::execute(const eckit::option::CmdArgs&) {

    mstro_cdo_selector selector = nullptr;

    mstro_status s =
        mstro_cdo_selector_create(nullptr, nullptr, "(.maestro.ecmwf.step = 2)", &selector);
    ASSERT(s == MSTRO_OK);

    mstro_subscription offer_subscription;
    s = mstro_subscribe(selector, MSTRO_POOL_EVENT_OFFER, MSTRO_SUBSCRIPTION_OPTS_REQUIRE_ACK,
                        &offer_subscription);
    ASSERT(s == MSTRO_OK);

    s = mstro_cdo_selector_dispose(selector);
    ASSERT(s == MSTRO_OK);

    mstro_subscription join_leave_subscription;
    auto flag = static_cast<mstro_pool_event_kinds>(
        (MSTRO_POOL_EVENT_APP_JOIN | MSTRO_POOL_EVENT_APP_LEAVE));
    mstro_subscribe(nullptr, flag, MSTRO_SUBSCRIPTION_OPTS_REQUIRE_ACK, &join_leave_subscription);

    eckit::Log::info() << " *** Start polling" << std::endl;
    bool done = false;
    while(not done) {
        mstro_pool_event event = nullptr;
        s = mstro_subscription_poll(join_leave_subscription, &event);
        ASSERT(s == MSTRO_OK);

        if (event != nullptr) {
            eckit::Log::info() << " *** Event: join/leave" << std::endl;
            mstro_pool_event tmp = event;
            while (tmp != nullptr) {
                switch (tmp->kind) {
                    case MSTRO_POOL_EVENT_APP_JOIN:
                        eckit::Log::info() << " *** Application " << tmp->join.component_name
                                           << " is joining" << std::endl;
                        break;
                    case MSTRO_POOL_EVENT_APP_LEAVE:
                        eckit::Log::info() << " *** Application " << tmp->leave.appid
                                           << " is leaving" << std::endl;
                        done = true;
                        eckit::Log::info() << " *** Terminating " << std::endl;
                        break;
                    default:
                        std::ostringstream os;
                        os << "Unexpected event " << tmp->kind;
                        throw eckit::SeriousBug{os.str()};
                }
                tmp = tmp->next;
            }
            s = mstro_subscription_ack(join_leave_subscription, event);
            ASSERT(s == MSTRO_OK);
            s = mstro_pool_event_dispose(event);
            ASSERT(s == MSTRO_OK);
        } else {
            s = mstro_subscription_poll(offer_subscription, &event);
            ASSERT(s == MSTRO_OK);
            // eckit::Log::info() << " *** Polling CDO events" << std::endl;
            if(event) {
                util::ScopedTimer timer{timing_};
                mstro_pool_event tmp = event;
                while (tmp) {
                    ++cdoEventCount_;
                    eckit::Log::info() << " *** CDO event ";
                    switch(tmp->kind) {
                    case MSTRO_POOL_EVENT_OFFER: {
                        eckit::Log::info()
                            << mstro_pool_event_description(tmp->kind)
                            << " occured -- cdo name: " << tmp->offer.cdo_name << std::endl;
                        mstro_cdo cdo = nullptr;
                        s = mstro_cdo_declare(tmp->offer.cdo_name, MSTRO_ATTR_DEFAULT, &cdo);
                        ASSERT(s == MSTRO_OK);
//                         eckit::Log::info() << " *** Require " << std::endl;
//                         s = mstro_cdo_require(cdo);
//                         ASSERT(s == MSTRO_OK);
//                        requiredCdos_.push_back(cdo);
//                        eckit::Log::info() << " *** Demand " << std::endl;
//                        s = mstro_cdo_demand(cdo);
//                        ASSERT(s == MSTRO_OK);

                        // eckit::Log::info() << " *** Retract " << std::endl;
                        // s = mstro_cdo_retract(cdo);
                        // ASSERT(s == MSTRO_OK);

//                        eckit::Log::info() << " *** Attribute get " << std::endl;

//                        auto valType = MSTRO_CDO_ATTR_VALUE_uint64;
//                        const size_t* sz;
//                        s = mstro_cdo_attribute_get(cdo, ".maestro.core.cdo.scope.local-size",
//                                                    &valType, reinterpret_cast<const void**>(&sz));

//                        eckit::Log::info()
//                            << " *** CDO with size " << *sz << " has been demanded" << std::endl;

                        // FileSink here
                        break;
                    }
                    default:
                        eckit::Log::info() << " *** CDO event "
                                           << mstro_pool_event_description(tmp->kind) << std::endl;
                    }
                    tmp = tmp->next;
                }
                s = mstro_subscription_ack(offer_subscription, event);
                ASSERT(s == MSTRO_OK);
                s = mstro_pool_event_dispose(event);
                ASSERT(s == MSTRO_OK);
            }
        }
    }

    eckit::Log::info() << " MaestroSyphon: detected " << cdoEventCount_
                       << " cdo events -- it has taken " << timing_.elapsed_ << "s" << std::endl;

}

}  // namespace multio

//---------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    multio::MaestroSyphon tool(argc, argv);
    return tool.start();
}
