
#ifndef multio_sandbox_Message_H
#define multio_sandbox_Message_H

#include <string>
#include <thread>
#include <vector>

namespace multio {
namespace sandbox {

enum class MsgTag
{
    mapping_data,
    field_data,
    step_complete,
    forecast_complete,
    open,
    close
};

struct Peer {
    Peer(int id) { id_ = id; }
    operator int() { return id_; }
    int id_;
};

inline bool operator==(const Peer& lhs, const Peer& rhs) {
    return lhs.id_ == rhs.id_;
}

inline bool operator!=(const Peer& lhs, const Peer& rhs) {
    return lhs.id_ != rhs.id_;
}

inline bool operator<(const Peer& lhs, const Peer& rhs) {
    return lhs.id_ < rhs.id_;
}

inline bool operator<=(const Peer& lhs, const Peer& rhs) {
    return lhs.id_ <= rhs.id_;
}

inline bool operator>(const Peer& lhs, const Peer& rhs) {
    return lhs.id_ >= rhs.id_;
}

inline bool operator>=(const Peer& lhs, const Peer& rhs) {
    return lhs.id_ >= rhs.id_;
}

class Message {
public:  // methods
    Message(size_t size = 0, Peer peer = -1, MsgTag tag = MsgTag::field_data);

    void* data();
    const void* data() const;

    size_t size() const;
    Peer peer() const;
    MsgTag tag() const;

    void peer(Peer new_peer);
    void resize(size_t);

    size_t read(void* buffer, size_t length) const;
    size_t write(const void* buffer, size_t length);

    std::string name() const;

private: // methods
    void print(std::ostream& out) const;
    friend std::ostream& operator<<(std::ostream& s, const Message& x) {
        x.print(s);
        return s;
    }

private: // members
    std::vector<char> payload_;

    Peer peer_;  // Source or destination depending on context
    MsgTag tag_;
    mutable size_t position_;
};

}  // namespace sandbox
}  // namespace multio

#endif
