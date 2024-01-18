
#pragma once

#include "multio/domain/MaskCompression.h"
#include "multio/message/Message.h"
#include "multio/message/Metadata.h"

#include <cstddef>
#include <cstdint>
#include <map>
#include <mutex>
#include <string>
#include <vector>

namespace eckit {
class LocalConfiguration;
}

namespace multio::domain {

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

    // const std::vector<bool>& get(const std::string& name) const;
    EncodedRunLengthPayload get(const std::string& name) const;

private:
    void addPartialMask(message::Message msg);

    bool allPartsArrived(const message::Message& msg) const;
    void createBitmask(message::Message msg);

    std::unordered_map<std::string, std::vector<message::Message>> messages_;
    // std::unordered_map<std::string, std::vector<bool>> bitmasks_;
    std::unordered_map<std::string, eckit::Buffer> bitmasks_;

    mutable std::mutex mutex_;
};

}  // namespace multio::domain
