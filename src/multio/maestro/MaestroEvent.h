#ifndef multio_MaestroEvent_H
#define multio_MaestroEvent_H

extern "C" {
#include <maestro.h>
}

namespace multio {

class MaestroEvent {
public:
    MaestroEvent(mstro_pool_event event);
    ~MaestroEvent();
    bool isNotNull() { return event_ != nullptr; }
    bool isNull() { return !isNotNull(); }
    mstro_pool_event get_mstro_pool_event() { return event_; }
private:
    mstro_pool_event event_;
};

}  // namespace multio

#endif  // multio_MaestroEvent_H
