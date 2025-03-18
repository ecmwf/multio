/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Philipp Geier

/// @date Jan 2024

#pragma once

#include "eckit/io/Buffer.h"
#include "eckit/serialisation/Stream.h"
#include "multio/util/TypeTraits.h"
#include "multio/util/VariantHelpers.h"

namespace multio::message {

//----------------------------------------------------------------------------------------------------------------------

struct PayloadReference {
    const void* data_;
    std::size_t size_;


    const void* data() const noexcept { return data_; }
    std::size_t size() const noexcept { return size_; }

    bool operator==(const PayloadReference& other) const noexcept {
        return (data_ == other.data_) && (size_ == other.size_);
    }
    bool operator!=(const PayloadReference& other) const noexcept {
        return (data_ != other.data_) || (size_ != other.size_);
    }


    operator const char*() const { return static_cast<const char*>(data_); };
    operator const void*() const { return data_; };
};

using SharedPayloadTypes = util::TypeList<std::shared_ptr<eckit::Buffer>, PayloadReference>;
using SharedPayloadVariant = util::ApplyTypeList_t<std::variant, SharedPayloadTypes>;


class SharedPayload : public SharedPayloadVariant {
public:
    using This = SharedPayload;
    using Base = SharedPayloadVariant;
    // ICPC has not sane conversion - constructor and assignment are explicitly redefined below
    // using SharedPayloadVariant::SharedPayloadVariant;
    // using Base::operator=;

    SharedPayload(const This& other) = default;
    SharedPayload(This&&) noexcept = default;

    This& operator=(const This& other) = default;
    This& operator=(This&& other) noexcept = default;

    const void* data() const;
    std::size_t size() const;

    // Might throw if holding a reference
    void* modifyData();


    operator PayloadReference() const;


    // Either reuses the existing shared_ptr if the buffer is unique, otherwise copy into a new buffer
    std::shared_ptr<eckit::Buffer> moveOrCopy();

    // Makes sure that this object is the only owner of the data and copy the data if it is not
    void acquire();

    //----------------------------------------------------------------------------------------------------------------------


    // Implementation details

    // Constructor that deals with all other cases exlusive to the unique_ptr handling - Sane conversion is
    // reimplemented for icpc
    template <typename T,
              std::enable_if_t<(!std::is_same_v<std::decay_t<T>, This> && !std::is_same_v<std::decay_t<T>, Base>), bool>
              = true>
    SharedPayload(T&& val) : Base(util::SaneOverloadResolutionResult_t<T, SharedPayloadTypes>{std::forward<T>(val)}) {};


    SharedPayload() : Base() {};


    // Constructor that is required for icpc....
    template <typename T,
              std::enable_if_t<(!std::is_same_v<std::decay_t<T>, This> && !std::is_same_v<std::decay_t<T>, Base>), bool>
              = true>
    This& operator=(T&& val) {
        using TI = util::SaneOverloadResolutionResult_t<T, SharedPayloadTypes>;
        if (this->index() == util::GetVariantIndex_v<TI, Base>) {
            std::get<TI>(*this) = std::forward<T>(val);
        }
        else {
            Base::operator=(TI{std::forward<T>(val)});
        }
        return *this;
    }
};


eckit::Stream& operator<<(eckit::Stream& strm, const SharedPayload& sp);


}  // namespace multio::message

//----------------------------------------------------------------------------------------------------------------------


namespace std {
template <>
struct variant_size<multio::message::SharedPayload> : variant_size<multio::message::SharedPayloadVariant> {};

template <std::size_t I>
struct variant_alternative<I, multio::message::SharedPayload>
    : variant_alternative<I, multio::message::SharedPayloadVariant> {};
}  // namespace std

namespace multio::util {

template <typename T>
struct GetVariantIndex<T, multio::message::SharedPayload> : GetVariantIndex<T, multio::message::SharedPayloadVariant> {
};

}  // namespace multio::util


//----------------------------------------------------------------------------------------------------------------------
