
#pragma once

#include "multio/message/BaseMetadata.h"
#include "multio/message/Message.h"


namespace multio::message {

static std::string PARAMETRIZATION_PAYLOAD_KEY = "payloadKey";

class Parametrization {
public:
    Parametrization() = default;

    Parametrization(const Parametrization& rhs) = delete;
    Parametrization(Parametrization&& rhs) noexcept = delete;

    Parametrization& operator=(const Parametrization& rhs) = delete;
    Parametrization& operator=(Parametrization&& rhs) noexcept = delete;

    static Parametrization& instance();

    BaseMetadata& get();

    // Update with no payload but key value pairs - throws if a key already exists with a different value
    void update(const BaseMetadata&);

    // Update a specific key with a vector of bytes as value
    void update(const std::string& key, const void* data, std::size_t size);

    // Update a specific key with a vector of bytes as value
    void update(const Message& msg);

    void clear();


private:
    void print(std::ostream& out) const;

    friend std::ostream& operator<<(std::ostream& s, const Parametrization& x) {
        x.print(s);
        return s;
    }
    void update(const std::string& key, const MetadataValue&);

    // TODO use RW lock (maybe with atomic flag)
    mutable std::mutex mutex_;

    BaseMetadata data_;
};

}  // namespace multio::message
