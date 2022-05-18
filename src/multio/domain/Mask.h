
#ifndef multio_server_Mask_H
#define multio_server_Mask_H

#include <cstddef>
#include <cstdint>
#include <map>
#include <mutex>
#include <vector>

#include <eckit/io/Buffer.h>

namespace eckit {
class LocalConfiguration;
}

namespace multio {

namespace message {
class Message;
class Metadata;
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

    static std::string key(const message::Metadata& md);

    void add(message::Message msg);

    const std::vector<bool>& get(const std::string& name) const;

private:

    void addPartialMask(const message::Message& msg);

    bool allPartsArrived(message::Message msg) const;
    void createBitmask(message::Message msg);

    std::map<std::string, std::vector<message::Message>> messages_;
    std::map<std::string, std::vector<bool>> bitmasks_;

    mutable std::mutex mutex_;
};

}  // namespace domain
}  // namespace multio

#endif
