
#include "TransportStatistics.h"

namespace multio {
namespace server {

TransportStatistics::TransportStatistics() {}

void multio::server::TransportStatistics::report(std::ostream& out, const char* indent) const {
    
    reportTime(out, " -- Waiting for buffer", waitTiming_, indent);

    reportBytes(out, " -- Sending data", sendSize_, indent);
    reportTime(out, " -- Send time (async)", isendTiming_, indent);
    reportTime(out, " -- Send time (block)", sendTiming_, indent);
    reportTime(out, " -- Serialise data", sendTiming_, indent);

    reportCount(out, " -- Receive calls:", receiveCount_, indent);
    reportBytes(out, " -- Receiving data:", receiveSize_, indent);
    reportTime(out, " -- Probing for data:", probeTiming_, indent);
    reportTime(out, " -- Receive timing:", receiveTiming_, indent);

    reportTime(out, " -- Push-queue timing:", pushToQueueTiming_, indent);
    reportTime(out, " -- Deserialise data:", decodeTiming_, indent);
    reportTime(out, " -- Returning data:", returnTiming_, indent);
    reportTime(out, " -- Total for return:", totReturnTiming_, indent);
}

}  // namespace server
}  // namespace multio
