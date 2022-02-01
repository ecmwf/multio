
#ifndef multio_server_Mask_H
#define multio_server_Mask_H

#include <cstddef>
#include <cstdint>
#include <map>
#include <mutex>
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

    const std::vector<uint8_t>& get(const std::string& name) const;

private:

    bool allPartsArrived(message::Message msg) const;

    std::map<std::string, std::vector<message::Message>> messages_;
    std::map<std::string, std::vector<uint8_t>> bitmasks_;

    mutable std::mutex mutex_;
};

}  // namespace domain
}  // namespace multio

#endif
