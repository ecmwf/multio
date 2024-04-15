
#include "TransportStatistics.h"

namespace multio::transport {

TransportStatistics::TransportStatistics() {}

void TransportStatistics::report(std::ostream& out, const char* indent) {

    reportTime(out, "    -- Waiting for buffer", waitTiming_, indent);

    reportCount(out, "    -- Send count (async)", isendCount_, indent);
    reportBytes(out, "    -- Sending data (async)", isendSize_, indent);
    reportTime(out, "    -- Send time (async)", isendTiming_, indent);

    reportCount(out, "    -- Send count (block)", sendCount_, indent);
    reportBytes(out, "    -- Sending data (block)", sendSize_, indent);
    reportTime(out, "    -- Send time (block)", sendTiming_, indent);
    double sendTime = sendTiming_.elapsedTimeSeconds();
    if (sendTime > 0.0) {
        reportRate(out, "    -- Send rate (block)", sendSize_ / sendTime, indent);
    }

    reportTime(out, "    -- Serialise data", encodeTiming_, indent);

    reportTime(out, "    -- Probing for data", probeTiming_, indent);
    reportCount(out, "    -- Receive count", receiveCount_, indent);
    reportBytes(out, "    -- Receiving data", receiveSize_, indent);
    reportTime(out, "    -- Receive timing", receiveTiming_, indent);
    double receiveTime = receiveTiming_.elapsedTimeSeconds();
    if (receiveTime > 0.0) {
        reportRate(out, "    -- Receive rate", receiveSize_ / receiveTime, indent);
    }

    reportTime(out, "    -- Push-queue timing", pushToQueueTiming_, indent);
    reportTime(out, "    -- Deserialise data", decodeTiming_, indent);
    reportTime(out, "    -- Returning data", returnTiming_, indent);
    reportTime(out, "    -- Total for return", totReturnTiming_, indent);
}

}  // namespace multio::transport
