
#include "TransportStatistics.h"

namespace multio {
namespace server {

TransportStatistics::TransportStatistics() {}

void multio::server::TransportStatistics::report(std::ostream& out, const char* indent) const {

    reportTime(out, "    -- Waiting for buffer", waitTiming_, indent);

    reportCount(out, "    -- Send count (async)", isendCount_, indent);
    reportBytes(out, "    -- Sending data (async)", isendSize_, indent);
    reportTime(out, "    -- Send time (async)", isendTiming_, indent);

    reportCount(out, "    -- Send count (block)", sendCount_, indent);
    reportBytes(out, "    -- Sending data (block)", sendSize_, indent);
    reportTime(out, "    -- Send time (block)", sendTiming_, indent);
    if (sendTiming_.elapsed_) {
        reportRate(out, "    -- Send rate (block)", sendSize_ / sendTiming_.elapsed_, indent);
    }

    reportTime(out, "    -- Serialise data", encodeTiming_, indent);

    reportTime(out, "    -- Probing for data", probeTiming_, indent);
    reportCount(out, "    -- Receive count", receiveCount_, indent);
    reportBytes(out, "    -- Receiving data", receiveSize_, indent);
    reportTime(out, "    -- Receive timing", receiveTiming_, indent);
    if (receiveTiming_.elapsed_) {
        reportRate(out, "    -- Receive rate", receiveSize_ / receiveTiming_.elapsed_, indent);
    }

    reportTime(out, "    -- Push-queue timing", pushToQueueTiming_, indent);
    reportTime(out, "    -- Deserialise data", decodeTiming_, indent);
    reportTime(out, "    -- Returning data", returnTiming_, indent);
    reportTime(out, "    -- Total for return", totReturnTiming_, indent);
}

}  // namespace server
}  // namespace multio
