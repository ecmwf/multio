
#ifndef multio_attic_MpiTransport_H
#define multio_attic_MpiTransport_H

#include <string>

#include "eckit/mpi/Comm.h"

#include "multio/attic/Transport.h"

namespace multio {
namespace attic {

class MpiTransport final : public Transport {
public:
    MpiTransport(const std::string& title, const size_t no_serv,
                 eckit::mpi::Comm& parent = eckit::mpi::comm("world"));
    ~MpiTransport() override;

    const eckit::mpi::Comm& clientComm() const;
    const eckit::mpi::Comm& atticComm() const;

private:
    void receive(Message& msg) const override;
    void send(const Message& message) const override;
    void synchronise() const override;

    size_t size() const override;
    bool server() const override;
    bool client() const override;

    size_t globalRank() const override;
    size_t clientRank() const override;

    void print(std::ostream &os) const override;

    void setClientCommAsDefault() const;
    void createCommunicators();

    const eckit::mpi::Comm& globalComm_;

    const eckit::mpi::Comm* clientComm_ = nullptr;
    const eckit::mpi::Comm* atticComm_ = nullptr;

};

}  // namespace attic
}  // namespace multio

#endif
