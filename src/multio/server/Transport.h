
#ifndef multio_server_Transport_H
#define multio_server_Transport_H

#include "eckit/memory/NonCopyable.h"

#include <string>

namespace multio {
namespace server {

class Message;

class Transport : public eckit::NonCopyable {
public: // methods

    Transport(const std::string& title, size_t no_serv);
    virtual ~Transport();

    virtual void notifyAllClients(const Message& msg) const = 0;

    virtual void receive(Message &message) const = 0;
    virtual void send(const Message &message) const = 0;
    virtual void synchronise() const = 0;

    virtual size_t size() const = 0;
    virtual bool server() const = 0;
    virtual bool client() const = 0;

    virtual size_t globalRank() const = 0;
    virtual size_t clientRank() const = 0;

    const std::string& title() const;
    size_t noServers() const;
    size_t noClients() const;

protected:

    const std::string title_;
    const size_t noServers_;

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
