
#include "MaestroSelector.h"

#include "eckit/exception/Exceptions.h"

namespace multio {

MaestroSelector::MaestroSelector(mstro_schema schema, const char* nspace, const char* query) {
    ASSERT(MSTRO_OK == mstro_cdo_selector_create(schema, nspace, query, &selector_));
}

MaestroSelector::~MaestroSelector() {
    ASSERT(MSTRO_OK == mstro_cdo_selector_dispose(selector_));
}

MaestroSubscription MaestroSelector::subscribe(mstro_pool_event_kind events,
                                               mstro_subscription_opts flags) {
    return MaestroSubscription{selector_, events, flags};
}

}  // namespace multio
