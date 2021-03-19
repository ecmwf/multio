
#include "MaestroSelector.h"

#include "eckit/exception/Exceptions.h"

namespace multio {

MaestroSelector::MaestroSelector(mstro_schema schema, const char* nspace, const char* query) :
    status_{mstro_cdo_selector_create(schema, nspace, query, &selector_)} {
    ASSERT(status_ == MSTRO_OK);
}

MaestroSelector::~MaestroSelector() {
    mstro_cdo_selector_dispose(selector_);
}

mstro_subscription MaestroSelector::subscribe(mstro_pool_event_kind events,
                                              mstro_subscription_opts flags) {
    mstro_subscription subscription;
    status_ = mstro_subscribe(selector_, events, flags, &subscription);
    ASSERT(status_ == MSTRO_OK);
    return subscription;
}

}  // namespace multio
