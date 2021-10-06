
#include "MaestroSubscription.h"

namespace multio {

MaestroSubscription::MaestroSubscription(mstro_cdo_selector selector, mstro_pool_event_kind events,
                                         mstro_subscription_opts flags, MaestroStatistics& statistics) :
    statistics_{statistics} {
    eckit::AutoTiming timing(statistics_.timer_, statistics_.subscriptionConstructionTiming_);
    ASSERT(MSTRO_OK ==  mstro_subscribe(selector, events, flags, &subscription_));
}

MaestroSubscription::~MaestroSubscription() {
    eckit::AutoTiming timing(statistics_.timer_, statistics_.subscriptionDestructionTiming_);
    ASSERT(MSTRO_OK == mstro_subscription_dispose(subscription_));
}

MaestroSubscription::MaestroSubscription(MaestroSubscription&& rhs) noexcept :
    subscription_{rhs.subscription_}, statistics_{rhs.statistics_} {
    eckit::AutoTiming timing(statistics_.timer_, statistics_.subscriptionPollTiming_);
    rhs.subscription_ = nullptr;
}

MaestroEvent MaestroSubscription::poll() {
    eckit::AutoTiming timing(statistics_.timer_, statistics_.subscriptionPollTiming_);
    mstro_pool_event event;
    ASSERT(MSTRO_OK == mstro_subscription_poll(subscription_, &event));
    return MaestroEvent{event, statistics_};
}

MaestroEvent MaestroSubscription::wait() {
    eckit::AutoTiming timing(statistics_.timer_, statistics_.subscriptionWaitTiming_);
    mstro_pool_event event;
    ASSERT(MSTRO_OK == mstro_subscription_wait(subscription_, &event));
    return MaestroEvent{event, statistics_};
}

MaestroEvent MaestroSubscription::timedwait(const timespec* abstime) {
    eckit::AutoTiming timing(statistics_.timer_, statistics_.subscriptionTimedWaitTiming_);
    mstro_pool_event event;
    mstro_status s = mstro_subscription_timedwait(subscription_, abstime, &event);
    ASSERT(s == MSTRO_OK || s == MSTRO_TIMEOUT);
    return MaestroEvent{event, statistics_};
}

bool MaestroSubscription::ack(MaestroEvent& event) {
    eckit::AutoTiming timing(statistics_.timer_, statistics_.subscriptionAckTiming_);
    mstro_status s = mstro_subscription_ack(subscription_, event.raw_event());
    ASSERT(s == MSTRO_OK || s == MSTRO_INVARG);
    return s == MSTRO_OK;
}

}  // namespace multio
