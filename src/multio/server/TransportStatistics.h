
#ifndef multio_server_TransportStatistics_H
#define multio_server_TransportStatistics_H

#include <iosfwd>

#include <eckit/log/Statistics.h>

namespace multio {
namespace server {

class TransportStatistics : public eckit::Statistics {
public:
    TransportStatistics();

    std::size_t isendCount_ = 0;
    std::size_t isendSize_ = 0;

    std::size_t sendCount_ = 0;
    std::size_t sendSize_ = 0;

    std::size_t receiveCount_ = 0;
    std::size_t receiveSize_ = 0;

    eckit::Timing waitTiming_;

    eckit::Timing isendTiming_;
    eckit::Timing sendTiming_;
    eckit::Timing encodeTiming_;

    eckit::Timing probeTiming_;
    eckit::Timing receiveTiming_;
    eckit::Timing pushToQueueTiming_;
    eckit::Timing decodeTiming_;
    eckit::Timing returnTiming_;
    eckit::Timing totReturnTiming_;

    void report(std::ostream &out, const char* indent = "") const;
};

}  // namespace server
}  // namespace multio

#endif // multio_server_TransportStatistics_H
