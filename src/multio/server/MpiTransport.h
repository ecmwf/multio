
#ifndef multio_server_MpiTransport_H
#define multio_server_MpiTransport_H

#include <string>

#include "eckit/mpi/Comm.h"

#include "multio/server/Transport.h"

namespace multio {
namespace server {

class MpiTransport final : public Transport {
public:
    MpiTransport(const std::string title, const size_t no_serv,
                 eckit::mpi::Comm& parent = eckit::mpi::comm("world"));
    ~MpiTransport() override;

    const eckit::mpi::Comm& clientComm() const;
    const eckit::mpi::Comm& serverComm() const;

private:
    void receiveFromClient(Message& message) const override;
    void sendToServer(const Message& message) const override;
    void synchronise() const override;

    size_t size() const override;
    bool server() const override;
    bool client() const override;

    size_t global_rank() const override;
    size_t client_rank() const override;

    void print(std::ostream &os) const override;

    void setClientCommAsDefault() const;
    void createCommunicators();

    const eckit::mpi::Comm& global_comm;

    const eckit::mpi::Comm* client_comm = nullptr;
    const eckit::mpi::Comm* server_comm = nullptr;

};

}  // namespace server
}  // namespace multio

#endif
