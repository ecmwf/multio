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
    bool isNull() { return event_ == nullptr; }
    operator bool() const { return event_ != nullptr; }
    mstro_pool_event raw_event() { return event_; }
private:
    mstro_pool_event event_;
};

}  // namespace multio

#endif  // multio_MaestroEvent_H
