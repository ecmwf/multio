
#include "MaestroSelector.h"

#include "eckit/exception/Exceptions.h"

namespace multio {

MaestroSelector::MaestroSelector(const char* query, MaestroStatistics& statistics, mstro_schema schema, const char* nspace) :
   statistics_{statistics} {
//    eckit::AutoTiming timing(statistics_.timer_, statistics_.selectorConstructionTiming_);
    ASSERT(MSTRO_OK == mstro_cdo_selector_create(schema, nspace, query, &selector_));
}

MaestroSelector::~MaestroSelector() {
//    eckit::AutoTiming timing(statistics_.timer_, statistics_.selectorDestructionTiming_);
    ASSERT(MSTRO_OK == mstro_cdo_selector_dispose(selector_));
}

MaestroSubscription MaestroSelector::subscribe(mstro_pool_event_kind events,
                                               mstro_subscription_opts flags) {
//    eckit::AutoTiming timing(statistics_.timer_, statistics_.selectorSubscribeTiming_);
    return MaestroSubscription{selector_, events, flags, statistics_};
}

}  // namespace multio
