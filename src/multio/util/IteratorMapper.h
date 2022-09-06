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

/// @date Aug 2022

/**
 * Template class that allows applying mappings lazily through iterators.
 * This is useful, for example, to avoid allocating to many `std::vector` to apply transformations.
 * A mapped container is self contained (i.e. can be passed arround) and further mappings can be
 * applied. The type can grow arbitrarily complex, but it can be efficient if the mapped values will
 * be moved or processed anyway.
 *
 * Todo:
 *  - Allow different kind of storage wrappers (i.e. references)
 *  - Implement map function to apply further compile time mappings
 * implementation)
 *
 */

#ifndef multio_util_IteratorMapper_H
#define multio_util_IteratorMapper_H

#include <iostream>
#include <iterator>

#include "eckit/utils/Optional.h"

namespace multio {
namespace util {

template <typename ForwardItContainer, class Mapper>
class MappedContainer;

template <typename ForwardItContainer, class Mapper, bool is_const>
using IteratorMapperValueType =
    typename std::conditional<is_const,
                              typename std::decay<decltype(std::declval<Mapper>()(
                                  *(std::declval<ForwardItContainer>().cbegin())))>::type const,
                              typename std::decay<decltype(std::declval<Mapper>()(
                                  *(std::declval<ForwardItContainer>().begin())))>::type>::type;

// TODO std::iterator is deprecated in c++17
template <typename ForwardItContainer, class Mapper, bool is_const = false>
class IteratorMapper
    : std::iterator<std::forward_iterator_tag,
                    IteratorMapperValueType<ForwardItContainer, Mapper, is_const>> {
public:
    using This = IteratorMapper<ForwardItContainer, Mapper, is_const>;
    using IteratorType = decltype(std::declval<ForwardItContainer>().cbegin());

    using iterator_category = std::forward_iterator_tag;
    using difference_type = std::ptrdiff_t;
    using value_type = IteratorMapperValueType<ForwardItContainer, Mapper, is_const>;
    using pointer = typename std::conditional<is_const, value_type const*, value_type*>::type;
    using reference = typename std::conditional<is_const, value_type const&, value_type&>::type;


    IteratorMapper(const This& other) :
        container_{other.container_}, mapper_{other.mapper_}, it_{other.it_}, val_{other.val_} {}

    IteratorMapper(This&& other) :
        container_{other.container_},
        mapper_{other.mapper_},
        it_{std::move(other.it_)},
        val_{std::move(other.val_)} {}

    reference operator=(const This& other) {
        if (it_ != other.it_) {
            container_ = other.container_;
            mapper_ = other.mapper_;
            it_ = other.it_;
            val_ = other.val_;
        }
        return *this;
    }
    reference operator=(This&& other) {
        if (it_ != other.it_) {
            container_ = other.container_;
            mapper_ = other.mapper_;
            it_ = std::move(other.it_);
            val_ = std::move(other.val_);
        }
        return *this;
    }

    reference operator*() const { return val_.value(); }
    reference operator*() { return val_.value(); }

    pointer operator->() const { return &val_.value(); }
    pointer operator->() { return &val_.value(); }

    This& operator++() {
        ++it_;

        if (it_ != container_.cend()) {
            val_ = mapper_(*it_);
        }
        return *this;
    }

    This operator++(int) {
        This current(container_, it_, mapper_, val_);
        ++(*this);
        return current;
    }

    bool operator==(const This& other) const noexcept { return it_ == other.it_; }

    bool operator!=(const This& other) const noexcept { return it_ != other.it_; }

private:
    template <typename ItType>
    IteratorMapper(ForwardItContainer const& container, Mapper const& mapper, ItType&& it,
                   const value_type& val) :
        container_(container), mapper_(mapper), it_(std::forward<IteratorType>(it)), val_{val} {}

    template <typename ItType>
    IteratorMapper(ForwardItContainer const& container, Mapper const& mapper, ItType&& it,
                   value_type&& val) :
        container_(container),
        mapper_(mapper),
        it_(std::forward<ItType>(it)),
        val_{std::move(val)} {}

    template <typename ItType>
    IteratorMapper(ForwardItContainer const& container, Mapper const& mapper, bool hasValue,
                   ItType&& it) :
        container_(container),
        mapper_(mapper),
        it_(std::forward<ItType>(it)),
        val_{hasValue ? eckit::Optional<value_type>{value_type(mapper(*it_))}
                      : eckit::Optional<value_type>{}} {}


    template <typename ItType>
    IteratorMapper(ForwardItContainer const& container, Mapper const& mapper, ItType&& it) :
        IteratorMapper(container, mapper, it != container.cend(), std::forward<ItType>(it)) {}

    IteratorMapper(ForwardItContainer const& container, Mapper const& mapper) :
        IteratorMapper(container, mapper, container.cbegin()) {}


    ForwardItContainer const& container_;
    Mapper const& mapper_;
    IteratorType it_;
    eckit::Optional<value_type> val_;

    friend class MappedContainer<ForwardItContainer, Mapper>;
};

template <typename ForwardItContainer, class Mapper>
class MappedContainer {
public:
    using iterator = IteratorMapper<ForwardItContainer, Mapper, false>;
    using const_iterator = IteratorMapper<ForwardItContainer, Mapper, true>;

    using This = MappedContainer<ForwardItContainer, Mapper>;

    template <typename Cont_, typename Mapper_>
    MappedContainer(Cont_&& container, Mapper_&& mapper) :
        container_(std::forward<Cont_>(container)), mapper_(std::forward<Mapper_>(mapper)) {}


    iterator begin() const { return iterator(container_, mapper_); }
    iterator end() const { return iterator(container_, mapper_, false, container_.end()); }

    iterator cbegin() const { return const_iterator(container_, mapper_); }
    iterator cend() const { return const_iterator(container_, mapper_, false, container_.end()); }

private:
    ForwardItContainer container_;
    Mapper mapper_;
};

}  // namespace util
}  // namespace multio

#endif
