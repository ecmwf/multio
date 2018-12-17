
#include "MpiTransport.h"

#include "multio/server/Message.h"

using eckit::mpi::Comm;

namespace multio {
namespace server {

namespace {
std::string prefix(const std::string& title) {
    return ("[" + title + "] ");
}
}  // namespace

MpiTransport::MpiTransport(const std::string& title, const size_t no_serv, Comm& parent) :
    Transport(title, no_serv),
    globalComm_(parent) {
    createCommunicators();
}

MpiTransport::~MpiTransport() = default;

void MpiTransport::receiveFromClient(Message& msg) const {
    auto status = globalComm_.probe(globalComm_.anySource(), globalComm_.anyTag());
    msg.reset(globalComm_.getCount<void>(status), status.source(), status.tag());
    globalComm_.receive<void>(msg.data(), msg.size(), msg.peer(), msg.tag());
}

void MpiTransport::sendToServer(const Message& msg) const {
    globalComm_.send<void>(msg.data(), msg.size(), msg.peer(), msg.tag());
}

void MpiTransport::synchronise() const {
    globalComm_.barrier();
}

size_t MpiTransport::size() const {
    return globalComm_.size();
}

bool MpiTransport::server() const {
    // Last ranks are designated as I/O servers
    return globalComm_.size() - noServers_ <= globalComm_.rank();
}

bool MpiTransport::client() const {
    return !server();
}

size_t MpiTransport::globalRank() const {
    return globalComm_.rank();
}

size_t MpiTransport::clientRank() const {
    ASSERT(clientComm_);
    return clientComm_->rank();
}

void MpiTransport::print(std::ostream& os) const {
    os << "MpiTransport[" << title() << "]";
}

const Comm& MpiTransport::clientComm() const {
    if (!client()) {
        throw eckit::UserError("Cannot access clientComm from non-client process.");
    }
    return *clientComm_;
}

const Comm& MpiTransport::serverComm() const {
    if (!server()) {
        throw eckit::UserError("Cannot access serverComm from non-server process.");
    }
    return *serverComm_;
}

void MpiTransport::setClientCommAsDefault() const {
    if (!client()) {
        throw eckit::UserError("Cannot access clientComm from non-client process.");
    }
    auto comm_name = prefix(title_) + "clients";
    eckit::mpi::setCommDefault(comm_name.c_str());
}

void MpiTransport::createCommunicators() {
    if (globalComm_.size() <= static_cast<size_t>(noServers_)) {
        throw eckit::BadParameter("Number of I/O processes must be lower than total processes");
    }

    auto comm_name = prefix(title_) + (server() ? "servers" : "clients");
    auto split     = &globalComm_.split(server(), comm_name);

    if (server()) {
        serverComm_ = split;
    } else {
        clientComm_ = split;
    }
}

}  // namespace server
}  // namespace multio
