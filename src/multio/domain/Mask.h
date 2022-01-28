
#ifndef multio_server_Mask_H
#define multio_server_Mask_H

#include <cstddef>
#include <cstdint>
#include <vector>

#include <eckit/io/Buffer.h>

namespace multio {

namespace message {
    class Message;
}

namespace domain {

class Mask {
public:
    Mask() = default;

    Mask(const Mask& rhs) = delete;
    Mask(Mask&& rhs) noexcept = delete;

    Mask& operator=(const Mask& rhs) = delete;
    Mask& operator=(Mask&& rhs) noexcept = delete;

    static Mask& instance();

    void add(message::Message msg);

    const DomainMap& get(const std::string& name) const;

private:

    mutable std::map<std::string, std::vector<Message>> messages_;
    mutable std::map<std::string, std::vector<bool>> bitmasks_;
};

}  // namespace domain
}  // namespace multio

#endif
