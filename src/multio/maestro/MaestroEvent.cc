
#include "MaestroEvent.h"

#include "eckit/exception/Exceptions.h"

namespace multio {

MaestroEvent::MaestroEvent(mstro_pool_event event) : event_{event} {
}

MaestroEvent::MaestroEvent(MaestroEvent&& rhs) : event_{rhs.event_} {
    rhs.event_ = nullptr;
}

MaestroEvent& MaestroEvent::operator=(MaestroEvent&& rhs) {
    if (this != &rhs) {
        dispose();
        event_ = rhs.event_;
        rhs.event_ = nullptr;
    }
    return *this;
}

MaestroEvent::~MaestroEvent() {
    dispose();
}

void MaestroEvent::dispose() {
    if (event_) {
        ASSERT(MSTRO_OK == mstro_pool_event_dispose(event_));
    }
}

}  // namespace multio
