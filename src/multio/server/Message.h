
#ifndef multio_server_Message_H
#define multio_server_Message_H

#include <string>
#include <vector>

#include <eckit/memory/NonCopyable.h>

#include "msg_tag.h"

namespace multio {
namespace server {

class Message {
public:  // methods
    Message(size_t size = 0, int peer = -1, int tag = msg_tag::field_data);

    void rewind() const;

    void* data();
    const void* data() const;

    size_t size() const;
    int peer() const;
    int tag() const;

    void reset(size_t size, int peer, int tag);
    void peer(const int new_peer);
    void resize(size_t);

    size_t read(void* buffer, size_t length) const;
    size_t write(const void* buffer, size_t length);

    std::string name() const;

private:
    void print(std::ostream& out) const;
    friend std::ostream& operator<<(std::ostream& s, const Message& x) {
        x.print(s);
        return s;
    }

    std::vector<char> buffer_;
    int peer_;  // Source or destination depending on context
    int tag_;
    mutable size_t position_;
};

}  // namespace server
}  // namespace multio

#endif
