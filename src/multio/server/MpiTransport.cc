
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

MpiTransport::MpiTransport(const std::string title, const size_t no_serv, Comm& parent) :
    Transport(std::move(title), no_serv),
    global_comm(parent) {
    createCommunicators();
}

MpiTransport::~MpiTransport() = default;

void MpiTransport::receiveFromClient(Message& msg) const {
    auto status = global_comm.probe(global_comm.anySource(), global_comm.anyTag());
    msg.reset(global_comm.getCount<void>(status), status.source(), status.tag());
    global_comm.receive<void>(msg.data(), msg.size(), msg.peer(), msg.tag());
}

void MpiTransport::sendToServer(const Message& msg) const {
    global_comm.send<void>(msg.data(), msg.size(), msg.peer(), msg.tag());
}

void MpiTransport::synchronise() const {
    global_comm.barrier();
}

size_t MpiTransport::size() const {
    return global_comm.size();
}

bool MpiTransport::server() const {
    // Last ranks are designated as I/O servers
    return global_comm.size() - no_servers_ <= global_comm.rank();
}

bool MpiTransport::client() const {
    return !server();
}

size_t MpiTransport::global_rank() const {
    return global_comm.rank();
}

size_t MpiTransport::client_rank() const {
    ASSERT(client_comm);
    return client_comm->rank();
}

void MpiTransport::print(std::ostream& os) const {
    os << "MpiTransport[" << title() << "]";
}

const Comm& MpiTransport::clientComm() const {
    if (!client()) {
        throw eckit::UserError("Cannot access clientComm from non-client process.");
    }
    return *client_comm;
}

const Comm& MpiTransport::serverComm() const {
    if (!server()) {
        throw eckit::UserError("Cannot access serverComm from non-server process.");
    }
    return *server_comm;
}

void MpiTransport::setClientCommAsDefault() const {
    if (!client()) {
        throw eckit::UserError("Cannot access clientComm from non-client process.");
    }
    auto comm_name = prefix(title_) + "clients";
    eckit::mpi::setCommDefault(comm_name.c_str());
}

void MpiTransport::createCommunicators() {
    if (global_comm.size() <= static_cast<size_t>(no_servers_)) {
        throw eckit::BadParameter("Number of I/O processes must be lower than total processes");
    }

    auto comm_name = prefix(title_) + (server() ? "servers" : "clients");
    auto split     = &global_comm.split(server(), comm_name);

    if (server()) {
        server_comm = split;
    } else {
        client_comm = split;
    }
}

}  // namespace server
}  // namespace multio
