
#ifndef multio_server_Transport_H
#define multio_server_Transport_H

#include "eckit/memory/NonCopyable.h"

#include <string>

namespace multio {
namespace server {

class Message;

class Transport : public eckit::NonCopyable {
public: // methods

    Transport(const std::string& title, const size_t no_serv);
    virtual ~Transport();

    virtual void receiveFromClient(Message& message) const = 0;
    virtual void sendToServer(const Message &message) const = 0;
    virtual void synchronise() const = 0;

    virtual size_t size() const = 0;
    virtual bool server() const = 0;
    virtual bool client() const = 0;

    virtual size_t global_rank() const = 0;
    virtual size_t client_rank() const = 0;

    const std::string& title() const;
    size_t no_servers() const;
    size_t no_clients() const;

protected:

    const std::string title_;
    const size_t no_servers_;

private: // methods

    virtual void print(std::ostream &os) const = 0;

    friend std::ostream& operator<<(std::ostream& os, const Transport& transport) {
        transport.print(os);
        return os;
    }
};

}  // namespace server
}  // namespace multio

#endif
