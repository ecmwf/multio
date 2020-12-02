/// @author Domokos Sarmany
/// @date   Dec 2020

#ifndef multio_MaestroSelector_H
#define multio_MaestroSelector_H

extern "C" {
#include <maestro.h>
}

namespace multio {

class MaestroSelector {
public:
    MaestroSelector(mstro_schema schema, const char* nspace, const char* query);
    ~MaestroSelector();

    mstro_subscription subscribe(mstro_pool_event_kind events, enum mstro_subscription_opts flags);

private:
    mstro_status status_ = MSTRO_NOENT;
    mstro_cdo_selector selector_ = nullptr;
};

}  // namespace multio

#endif  // multio_MaestroSelector_H
