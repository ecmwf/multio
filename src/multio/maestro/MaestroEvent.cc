
#include "MaestroEvent.h"

#include "eckit/exception/Exceptions.h"

namespace multio {

MaestroEvent::MaestroEvent(mstro_pool_event event, MaestroStatistics& statistics) :
    event_{event}, statistics_{statistics} {
    eckit::AutoTiming timing(statistics_.timer_, statistics_.eventConstructionTiming_);
}

MaestroEvent::MaestroEvent(MaestroEvent&& rhs) :
    event_{rhs.event_}, statistics_{rhs.statistics_} {
    eckit::AutoTiming timing(statistics_.timer_, statistics_.eventMoveTiming_);
    rhs.event_ = nullptr;
}

MaestroEvent& MaestroEvent::operator=(MaestroEvent&& rhs) {
    eckit::AutoTiming timing(statistics_.timer_, statistics_.eventMoveTiming_);
    if (this != &rhs) {
        dispose();
        event_ = rhs.event_;
        rhs.event_ = nullptr;
        statistics_ = std::move(rhs.statistics_);
    }
    return *this;
}

MaestroEvent::~MaestroEvent() {
    eckit::AutoTiming timing(statistics_.timer_, statistics_.eventDestructionTiming_);
    dispose();
}

void MaestroEvent::dispose() {
    eckit::AutoTiming timing(statistics_.timer_, statistics_.eventDisposeTiming_);
    if (event_) {
        eckit::AutoTiming timing(statistics_.timer_, statistics_.eventNonNullDisposeTiming_);
        ASSERT(MSTRO_OK == mstro_pool_event_dispose(event_));
    }
}

}  // namespace multio
