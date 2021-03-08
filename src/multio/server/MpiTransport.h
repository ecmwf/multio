/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany
/// @author Simon Smart
/// @author Tiago Quintino

/// @date Jan 2019

#ifndef multio_server_MpiTransport_H
#define multio_server_MpiTransport_H

#include <queue>
#include <fstream>

#include "eckit/io/Buffer.h"
#include "eckit/log/Statistics.h"
#include "eckit/mpi/Comm.h"
#include "eckit/serialisation/ResizableMemoryStream.h"

#include "multio/server/Transport.h"
#include "multio/server/StreamPool.h"

namespace multio {
namespace server {

class MpiTransport final : public Transport {
public:
    MpiTransport(const eckit::Configuration& config);
    ~MpiTransport();

private:
    Message receive() override;

    void send(const Message& msg) override;

    void print(std::ostream& os) const override;

    Peer localPeer() const override;

    const eckit::mpi::Comm& comm() const;

    eckit::mpi::Status blockingProbe();
    size_t blockingReceive();

    MpiPeer local_;

    eckit::Buffer buffer_;

    StreamPool pool_;

    std::queue<Message> msgPack_;

    eckit::Timing totSendTiming_;
    eckit::Timing receiveTiming_;
    eckit::Timing probeTiming_;
    eckit::Timing totReceiveTiming_;

    std::size_t bytesReceived_ = 0;

    std::ofstream log_;
};

}  // namespace server
}  // namespace multio

#endif
