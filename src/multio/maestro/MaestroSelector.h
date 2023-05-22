/// @author Domokos Sarmany
/// @date   Dec 2020

#pragma once

#include "MaestroSubscription.h"
extern "C" {
#include <maestro.h>
}

namespace multio {

class MaestroSelector {
public:
    MaestroSelector(const char* query, mstro_schema schema = nullptr, const char* nspace = nullptr);
    ~MaestroSelector();

    MaestroSubscription subscribe(mstro_pool_event_kind events, enum mstro_subscription_opts flags);

private:
    mstro_cdo_selector selector_ = nullptr;
};

}  // namespace multio
