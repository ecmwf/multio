
#pragma once

#include "multio/message/BaseMetadata.h"
#include "multio/message/Message.h"


namespace multio::message {

static std::string PARAMETRIZATION_PAYLOAD_KEY = "payloadKey";

/** Global singleton metadata object that contains information that may be send once at the beginning of a run
 *
 *  Important: Key-value pairs are ment to be constant. It is possible to update a key with the same value multiple
 * times. However, to enforce consistency a key can not be updated with a different value - this will throw an exception
 * to notify about inconsistency.
 *
 *             This singleton object is shared accross multiple multio instances (e.g. IFS and NEMO). Hence it is
 * assumed that all global key-value pairs of different models are exclusive to each other or contain the same value.
 *
 */
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
