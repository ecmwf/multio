
#include "eckit/container/Queue.h"
#include "eckit/config/Resource.h"
#include "eckit/log/Log.h"
#include "eckit/option/SimpleOption.h"

#include "multio/LibMultio.h"
#include "multio/maestro/CdoNamer.h"
#include "multio/maestro/MaestroCdo.h"
#include "multio/maestro/MaestroSelector.h"
#include "multio/maestro/MaestroStatistics.h"
#include "multio/maestro/MaestroWorker.h"
#include "multio/maestro/ThreadsafeMap.h"
#include "multio/tools/MultioTool.h"
#include "multio/util/ScopedTimer.h"

#include "pgen/prodgen/BatchGenerator.h"
#include "pgen/prodgen/Requirement.h"
#include "pgen/prodgen/RequirementGenerator.h"

using multio::LibMultio;

namespace multio {

class MaestroSyphon final : public multio::MultioTool, public pgen::RequirementConsumer {
public:  // methods
    MaestroSyphon(int argc, char** argv);
    ~MaestroSyphon() {
        statistics_.report(eckit::Log::info());
    }
private:
    void usage(const std::string& tool) const override {
        eckit::Log::info() << std::endl << "Usage: " << tool << " [options]" << std::endl;
    }

    void init(const eckit::option::CmdArgs& args) override;

    void finish(const eckit::option::CmdArgs&) override;

    void execute(const eckit::option::CmdArgs& args) override;

    virtual void consume(const pgen::Requirement&, size_t) override;

    void process_join_leave_events(MaestroSubscription& join_leave_subscription);

    void process_offer_events(MaestroSubscription& offer_subscription);

    void broker();

    CdoNamer cdo_namer_;
    int number_ = 0;
    int nworkers_ = 1;
    unsigned cdoEventCount_ = 0;
    eckit::Timing timing_;
    bool compiled_ = false;
    std::unique_ptr<pgen::BatchGenerator> generator_;
    std::unordered_map<std::string, pgen::Requirement> requirements_;
    int req_count_ = 0;
    eckit::Queue<pgen::Requirement> req_queue_;
    MaestroStatistics statistics_;
};

MaestroSyphon::MaestroSyphon(int argc, char** argv) : 
    multio::MultioTool(argc, argv),
    req_queue_{eckit::Resource<size_t>("multioMessageQueueSize;$MULTIO_MESSAGE_QUEUE_SIZE", 1024)} {

    options_.push_back(new eckit::option::SimpleOption<std::string>("class", "Class (default od)"));
    options_.push_back(new eckit::option::SimpleOption<long>("date", "Data date (default today)"));
    options_.push_back(new eckit::option::SimpleOption<std::string>("expver", "Expver (default 0001)"));
    options_.push_back(new eckit::option::SimpleOption<uint64_t>("number", "Ensemble number"));
    options_.push_back(new eckit::option::SimpleOption<bool>("compiled", "Batch generator"));
    options_.push_back(new eckit::option::SimpleOption<uint64_t>("nworkers", "Number of threaded workers"));
}

void MaestroSyphon::init(const eckit::option::CmdArgs& args) {
    eckit::AutoTiming timing(statistics_.timer_, statistics_.syphonInitTiming_);
    args.get("number", number_);
    args.get("nworkers", nworkers_);
    args.get("compiled", compiled_);

    generator_.reset(pgen::BatchGeneratorFactory::build(compiled_ ? "binary" : "text", args));

    ASSERT(MSTRO_OK == mstro_init(::getenv("MSTRO_WORKFLOW_NAME"), ::getenv("MSTRO_COMPONENT_NAME"), 0));
}

void MaestroSyphon::finish(const eckit::option::CmdArgs&) {
    eckit::AutoTiming timing(statistics_.timer_, statistics_.syphonFinishTiming_);
    ASSERT(MSTRO_OK == mstro_finalize());
}

void MaestroSyphon::execute(const eckit::option::CmdArgs& args) {
    eckit::AutoTiming timing(statistics_.timer_, statistics_.syphonExecuteTiming_);
    LOG_DEBUG_LIB(LibMultio) << "*** Parsing requirements ***" << std::endl;
    for (size_t i = 0; i < args.count(); i++) {
        LOG_DEBUG_LIB(LibMultio) << args(i) << " ==> " << std::endl;
        std::unique_ptr<pgen::RequirementGenerator> parser(pgen::RequirementGeneratorFactory::build(args(i), args));
        parser->execute(*this);
    }

    for (auto& elem : requirements_)
        LOG_DEBUG_LIB(LibMultio) << elem.first << std::endl;
    LOG_DEBUG_LIB(LibMultio) << std::endl;

    std::vector<std::thread> worker_threads;

    for (int i = 0; i < nworkers_; i++)
        worker_threads.emplace_back(execute_worker, std::cref(args), std::ref(req_queue_));
    broker();
    for (int i = 0; i < nworkers_; i++)
        worker_threads[i].join();

    LOG_DEBUG_LIB(LibMultio) << " MaestroSyphon: detected " << cdoEventCount_
                             << " cdo events -- it has taken " << timing_.elapsed_ << "s" << std::endl;
}

void MaestroSyphon::consume(const pgen::Requirement& req, size_t)  {
    eckit::AutoTiming timing(statistics_.timer_, statistics_.syphonConsumeTiming_);
    auto cdo_name = cdo_namer_.name(req.retrieve());
    requirements_.insert({cdo_name, req});
}

void MaestroSyphon::process_join_leave_events(MaestroSubscription& join_leave_subscription) {
    eckit::AutoTiming timing(statistics_.timer_, statistics_.syphonJoinLeaveTiming_);
    auto event = join_leave_subscription.poll();
    if (event) {
        LOG_DEBUG_LIB(LibMultio) << " *** Event: join/leave" << std::endl;
        auto tmp = event.raw_event();
        while (tmp) {
            switch (tmp->kind) {
                case MSTRO_POOL_EVENT_APP_JOIN:
                LOG_DEBUG_LIB(LibMultio) << " *** Application " << tmp->join.component_name
                                             << " is joining" << std::endl;
                    break;
                case MSTRO_POOL_EVENT_APP_LEAVE:
                LOG_DEBUG_LIB(LibMultio) << " *** Application " << tmp->leave.appid
                                             << " is leaving" << std::endl;
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
}

void MaestroSyphon::process_offer_events(MaestroSubscription& offer_subscription) {
    eckit::AutoTiming timing(statistics_.timer_, statistics_.syphonOfferTiming_);
    auto event = offer_subscription.poll();
    if (event) {
        util::ScopedTimer timer{timing_};
        auto tmp = event.raw_event();
        while (tmp) {
            ++cdoEventCount_;
            LOG_DEBUG_LIB(LibMultio) << " *** CDO event ";
            LOG_DEBUG_LIB(LibMultio)
                << mstro_pool_event_description(tmp->kind)
                << " occured -- cdo name: " << tmp->offer.cdo_name << std::endl;
            auto offered_cdo = std::string(tmp->offer.cdo_name);
            auto it = requirements_.find(offered_cdo);
            if (it != requirements_.end()) {
                CdoMap::instance().emplace(offered_cdo, offered_cdo);
                auto& broker_cdo = CdoMap::instance().at(offered_cdo);
                broker_cdo.require();

                req_queue_.push(it->second);
                req_count_++;
            }
            tmp = tmp->next;
        }
        offer_subscription.ack(event);
    }
}

void MaestroSyphon::broker() {
    eckit::AutoTiming timing(statistics_.timer_, statistics_.syphonBrokerTiming_);
    LOG_DEBUG_LIB(LibMultio) << "*** Hi from broker" << std::endl;
    LOG_DEBUG_LIB(LibMultio) << "requirements: " << requirements_.size() << std::endl;
    std::string query{"(.maestro.ecmwf.number = " + std::to_string(number_) + ")"};

    MaestroSelector selector{query.c_str()};

    auto offer_subscription = selector.subscribe(MSTRO_POOL_EVENT_OFFER,
            MSTRO_SUBSCRIPTION_OPTS_SIGNAL_BEFORE|MSTRO_SUBSCRIPTION_OPTS_REQUIRE_ACK);

    MaestroSelector match_all_selector{nullptr};
    auto join_leave_subscription = match_all_selector.subscribe(
            MSTRO_POOL_EVENT_APP_JOIN|MSTRO_POOL_EVENT_APP_LEAVE,
            MSTRO_SUBSCRIPTION_OPTS_REQUIRE_ACK);

    LOG_DEBUG_LIB(LibMultio) << " *** Start polling" << std::endl;
    bool processed_requirements{false};
    while (not processed_requirements) {
        process_join_leave_events(join_leave_subscription);
        process_offer_events(offer_subscription);
        processed_requirements = (req_count_ == requirements_.size());
    }
    req_queue_.close();
    LOG_DEBUG_LIB(LibMultio) << "*** Broker is leaving" << std::endl;
}

}  // namespace multio

//---------------------------------------------------------------------------------------------------------------

int main(int argc, char** argv) {
    multio::MaestroSyphon tool(argc, argv);
    return tool.start();
}
