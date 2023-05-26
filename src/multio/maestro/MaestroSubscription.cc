
#include "MaestroSubscription.h"

namespace multio {

MaestroSubscription::MaestroSubscription(mstro_cdo_selector selector, mstro_pool_event_kind events,
                                         mstro_subscription_opts flags) {
    ASSERT(MSTRO_OK == mstro_subscribe(selector, events, flags, &subscription_));
}

MaestroSubscription::~MaestroSubscription() {
    ASSERT(MSTRO_OK == mstro_subscription_dispose(subscription_));
}

MaestroSubscription::MaestroSubscription(MaestroSubscription&& rhs) noexcept : subscription_{rhs.subscription_} {
    rhs.subscription_ = nullptr;
}

MaestroEvent MaestroSubscription::poll() {
    mstro_pool_event event;
    ASSERT(MSTRO_OK == mstro_subscription_poll(subscription_, &event));
    if (event != nullptr) {}
    return MaestroEvent{event};
}

MaestroEvent MaestroSubscription::wait() {
    mstro_pool_event event;
    ASSERT(MSTRO_OK == mstro_subscription_wait(subscription_, &event));
    return MaestroEvent{event};
}

MaestroEvent MaestroSubscription::timedwait(const timespec* abstime) {
    mstro_pool_event event;
    mstro_status s = mstro_subscription_timedwait(subscription_, abstime, &event);
    ASSERT(s == MSTRO_OK || s == MSTRO_TIMEOUT);
    return MaestroEvent{event};
}

bool MaestroSubscription::ack(MaestroEvent& event) {
    mstro_status s = mstro_subscription_ack(subscription_, event.raw_event());
    ASSERT(s == MSTRO_OK || s == MSTRO_INVARG);
    return s == MSTRO_OK;
}

}  // namespace multio
