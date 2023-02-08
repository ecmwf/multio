#pragma once

extern "C" {
#include <maestro.h>
}

namespace multio {

class MaestroEvent {
public:
    MaestroEvent(mstro_pool_event event);
    MaestroEvent(const MaestroEvent&) = delete;
    MaestroEvent& operator=(const MaestroEvent&) = delete;
    MaestroEvent(MaestroEvent&&);
    MaestroEvent& operator=(MaestroEvent&&);
    ~MaestroEvent();
    bool isNull() { return event_ == nullptr; }
    operator bool() const { return event_ != nullptr; }
    mstro_pool_event raw_event() { return event_; }

private:
    void dispose();
    mstro_pool_event event_;
};

}  // namespace multio
