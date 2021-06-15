
#ifndef multio_server_TransportStatistics_H
#define multio_server_TransportStatistics_H

#include <iosfwd>

#include <eckit/log/Statistics.h>

namespace multio {
namespace server {

class TransportStatistics : public eckit::Statistics {
public:
    TransportStatistics();

    std::size_t sendCount_;
    std::size_t receiveCount_;
    std::size_t sendSize_;
    std::size_t receiveSize_;

    eckit::Timing sendTiming_;
    eckit::Timing totSendTiming_;
    eckit::Timing receiveTiming_;
    eckit::Timing probeTiming_;
    eckit::Timing pushToQueueTiming_;
    eckit::Timing decodeTiming_;
    eckit::Timing returnTiming_;
    eckit::Timing totReturnTiming_;

    void report(std::ostream &out, const char* indent = "") const;
};

}  // namespace server
}  // namespace multio

#endif // multio_server_TransportStatistics_H
