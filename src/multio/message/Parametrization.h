
#pragma once

#include "multio/message/BaseMetadata.h"
#include "multio/message/Message.h"

#include "multio/message/Message.h"

#include <cstdint>


namespace multio::message {

static std::string PARAMETRIZATION_PAYLOAD_KEY = "payloadKey";
static std::string PARAMETRIZATION_PAYLOAD_ELEMENT_TYPE = "payloadElementType";

namespace parametrization {

/** Enum class describing supported types when sending arrays
 *
 */
enum class ElementType : unsigned long
{
    Byte,
    Int32,
    Int64,
    Real32,
    Real64,
};

ElementType decodeElementType(std::string_view sv);
std::string toString(ElementType);


template <ElementType>
struct GetElementType;

template <ElementType t>
using GetElementType_t = typename GetElementType<t>::type;

template <>
struct GetElementType<ElementType::Byte> {
    using type = unsigned char;
};
template <>
struct GetElementType<ElementType::Int32> {
    using type = std::int32_t;
};
template <>
struct GetElementType<ElementType::Int64> {
    using type = std::int64_t;
};
template <>
struct GetElementType<ElementType::Real32> {
    using type = float;
};
template <>
struct GetElementType<ElementType::Real64> {
    using type = double;
};

}  // namespace parametrization


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
    void update(std::string_view key, parametrization::ElementType elementType, const void* data, std::size_t size);
    void update(std::string_view key, std::string_view elementType, const void* data, std::size_t size);

    // Update a specific key with a vector of bytes as value
    void update(const Message& msg);

    void clear();


private:
    void print(std::ostream& out) const;

    friend std::ostream& operator<<(std::ostream& s, const Parametrization& x) {
        x.print(s);
        return s;
    }
    void update(std::string_view key, const MetadataValue&);

    // TODO use RW lock (maybe with atomic flag)
    mutable std::mutex mutex_;

    BaseMetadata data_;
};

}  // namespace multio::message
