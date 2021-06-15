
#include "TransportStatistics.h"

namespace multio {
namespace server {

TransportStatistics::TransportStatistics() {}

void multio::server::TransportStatistics::report(std::ostream& out, const char* indent) const {
    reportCount(out, "", sendCount_, indent);
    reportBytes(out, "", sendSize_, indent);
    reportTime(out, "", sendTiming_, indent);

    reportCount(out, "", receiveCount_, indent);
    reportBytes(out, "", receiveSize_, indent);
    reportTime(out, "", probeTiming_, indent);
    reportTime(out, "", receiveTiming_, indent);

    reportTime(out, "", pushToQueueTiming_, indent);
    reportTime(out, "", decodeTiming_, indent);
    reportTime(out, "", returnTiming_, indent);
    reportTime(out, "", totReturnTiming_, indent);
}

}  // namespace server
}  // namespace multio
