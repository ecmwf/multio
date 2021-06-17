
#include "MaestroEvent.h"

#include "eckit/exception/Exceptions.h"

namespace multio {

MaestroEvent::MaestroEvent(mstro_pool_event event) : event_{event} {}

MaestroEvent::~MaestroEvent() {
    if (event_ != nullptr)
        ASSERT(MSTRO_OK == mstro_pool_event_dispose(event_));
}

}  // namespace multio
