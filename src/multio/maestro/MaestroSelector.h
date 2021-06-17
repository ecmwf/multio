/// @author Domokos Sarmany
/// @date   Dec 2020

#ifndef multio_MaestroSelector_H
#define multio_MaestroSelector_H

#include "MaestroSubscription.h"
extern "C" {
#include <maestro.h>
}

namespace multio {

class MaestroSelector {
public:
    MaestroSelector(mstro_schema schema, const char* nspace, const char* query);
    MaestroSelector(const char* query) : MaestroSelector(nullptr, nullptr, query) {}
    ~MaestroSelector();

    MaestroSubscription subscribe(mstro_pool_event_kind events, enum mstro_subscription_opts flags);

private:
    mstro_cdo_selector selector_ = nullptr;
};

}  // namespace multio

#endif  // multio_MaestroSelector_H
