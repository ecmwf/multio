
extern "C" {
#include <maestro.h>
}

#include <algorithm>

#include "eccodes.h"

#include "eckit/io/DataHandle.h"
#include "eckit/log/Log.h"
#include "eckit/log/Statistics.h"
#include "eckit/message/Message.h"
#include "eckit/option/SimpleOption.h"

#include "metkit/codes/CodesContent.h"

#include "multio/tools/MultioTool.h"
#include "multio/util/ScopedTimer.h"

#include "multio/maestro/MaestroCdo.h"
#include "multio/maestro/MaestroSelector.h"
#include "multio/maestro/MaestroSubscription.h"
#include "multio/maestro/MaestroEvent.h"

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
    unsigned cdoEventCount_ = 0;
    eckit::Timing timing_;
};

MaestroSyphon::MaestroSyphon(int argc, char** argv) : multio::MultioTool(argc, argv) {
    options_.push_back(
        new eckit::option::SimpleOption<std::string>("file", "File containing the requirements"));
}

void MaestroSyphon::init(const eckit::option::CmdArgs& args) {
    args.get("file", reqFile_);

    ASSERT(MSTRO_OK == mstro_init(::getenv("MSTRO_WORKFLOW_NAME"), ::getenv("MSTRO_COMPONENT_NAME"), 0));
}

void MaestroSyphon::finish(const eckit::option::CmdArgs&) {
    ASSERT(MSTRO_OK == mstro_finalize());
}

void MaestroSyphon::execute(const eckit::option::CmdArgs&) {

    std::unique_ptr<eckit::DataHandle> handle{
        eckit::PathName{"consumed.grib"}.fileHandle(false)};
    handle->openForAppend(0);

    MaestroSelector selector{"(has .maestro.ecmwf.step)"};

    auto offer_subscription = selector.subscribe(MSTRO_POOL_EVENT_OFFER,
            MSTRO_SUBSCRIPTION_OPTS_REQUIRE_ACK);

    MaestroSelector match_all_selector{nullptr};
    auto join_leave_subscription = match_all_selector.subscribe(
            MSTRO_POOL_EVENT_APP_JOIN|MSTRO_POOL_EVENT_APP_LEAVE,
            MSTRO_SUBSCRIPTION_OPTS_REQUIRE_ACK);

    eckit::Log::info() << " *** Start polling" << std::endl;
    bool done = false;
    while(not done) {
        auto event = join_leave_subscription.poll();

        if (event) {
            eckit::Log::info() << " *** Event: join/leave" << std::endl;
            mstro_pool_event tmp = event.raw_event();
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
            join_leave_subscription.ack(event);
        } else {
            event = offer_subscription.poll();
            //eckit::Log::info() << " *** Polling CDO events" << std::endl;
            if(event) {
                util::ScopedTimer timer{timing_};
                mstro_pool_event tmp = event.raw_event();
                while (tmp) {
                    ++cdoEventCount_;
                    eckit::Log::info() << " *** CDO event ";
                    switch(tmp->kind) {
                    case MSTRO_POOL_EVENT_OFFER: {
                        eckit::Log::info()
                            << mstro_pool_event_description(tmp->kind)
                            << " occured -- cdo name: " << tmp->offer.cdo_name << std::endl;

                        MaestroCdo cdo{tmp->offer.cdo_name};

                        eckit::Log::info() << " *** Require " << std::endl;
                        cdo.require();

//                        eckit::Log::info() << " *** Retract " << std::endl;
//                        cdo.retract();

                        eckit::Log::info() << " *** Demand " << std::endl;
                        cdo.demand();

                        eckit::Log::info() << " *** Attribute get " << std::endl;

//                        mmbArray* mamba_ptr = NULL;
//                        s = mstro_cdo_access_mamba_array(cdo, &mamba_ptr);
//                        eckit::Log::info() << " *** CDO with size " << *sz << " and mamba pointer "
//                                           << mamba_ptr << " has been demanded " << std::endl;

                        const void* data = cdo.data();
                        int64_t sz = cdo.size();
                        eckit::Log::info() << " *** CDO with size " << sz << " and access pointer "
                                           << data << " has been demanded " << std::endl;

                        codes_handle* h = codes_handle_new_from_message(nullptr, data, sz);
                        eckit::message::Message msg{new metkit::codes::CodesContent{h, true}};
                        msg.write(*handle);

                        break;
                    }
                    default:
                        eckit::Log::info() << " *** CDO event "
                                           << mstro_pool_event_description(tmp->kind) << std::endl;
                    }
                    tmp = tmp->next;
                }
                offer_subscription.ack(event);
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
