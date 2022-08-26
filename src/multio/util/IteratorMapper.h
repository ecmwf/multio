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
 * A mapped container is self contained (i.e. can be passed arround) and further mappings can be applied.
 * The type can grow arbitrarily complex, but it can be efficient if the mapped values will be moved or processed anyway.
 *
 * Todo:
 *  - Allow different kind of storage wrappers (i.e. references)
 *  - Implement map function to apply further compile time mappings
 *  - Refactor by using an efficient optional implementation using union. (i.e. separate implementation)
 *
 */

#ifndef multio_util_IteratorMapper_H
#define multio_util_IteratorMapper_H

#include <iterator>
#include <iostream>

namespace multio {
namespace util {

template <typename ForwardItContainer, class Mapper>
class MappedContainer;

template <typename ForwardItContainer, class Mapper, bool is_const>
using IteratorMapperValueType = typename std::conditional<
    is_const,
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
        container_{other.container_},
        mapper_{other.mapper_},
        hasValue_{other.hasValue_},
        it_{other.it_},
        val_{.none = None{}} {
        if (hasValue_) {
            // Always use new (&val_.some) to assign a new value. Using the = assignment would cause a segfault for non-trivial classes
            new (&val_.some) value_type(other.val_.some);
        }
    }

    IteratorMapper(This&& other) :
        container_{other.container_},
        mapper_{other.mapper_},
        hasValue_{other.hasValue_},
        it_{std::move(other.it_)},
        val_{.none = None{}} {
        if (hasValue_) {
            // Always use new (&val_.some) to assign a new value. Using the = assignment would cause a segfault for non-trivial classes
            new (&val_.some) value_type(std::move(other.val_.some));
        }
    }

    reference operator=(const This& other) {
        if (it_ != other.it_) {
            bool hasValBefore = hasValue_;
            container_ = other.container_;
            mapper_ = other.mapper_;
            hasValue_ = other.hasValue_;
            it_ = other.it_;
            if(hasValBefore) {
                val_.some.~value_type();
            }
            if (hasValue_) {
                // Always use new (&val_.some) to assign a new value. Using the = assignment would cause a segfault for non-trivial classes
                new (&val_.some) value_type(other.val_.some_);
            }
            else {
                val_.none = None{};
            }
        }
        return *this;
    }
    reference operator=(This&& other) {
        if (it_ != other.it_) {
            bool hasValBefore = hasValue_;
            container_ = other.container_;
            mapper_ = other.mapper_;
            hasValue_ = other.hasValue_;
            it_ = std::move(other.it_);
            if(hasValBefore) {
                val_.some.~value_type();
            }
            if (hasValue_) {
                // Always use new (&val_.some) to assign a new value. Using the = assignment would cause a segfault for non-trivial classes
                new (&val_.some) value_type(std::move(other.val_.some_));
            }
            else {
                val_.none = None{};
            }
        }
        return *this;
    }

    reference operator*() const { return val_.some; }
    reference operator*()       { return val_.some; }

    pointer operator->() const { return &val_.some; }
    pointer operator->()       { return &val_.some; }

    This& operator++() {
        // std::cout << "Incr" <<std::endl;
        
        if(hasValue_) {
            // deallocate first
            val_.some.~value_type();
            hasValue_ = false;
        }
        ++it_;
        
        if(it_ != container_.cend()) {
            // Always use new (&val_.some) to assign a new value. Using the = assignment would cause a segfault for non-trivial classes
           new (&val_.some) value_type(mapper_(*it_));
           hasValue_ = true;
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

    ~IteratorMapper() {
        // std::cout << "destruct" << std::endl;
        if (hasValue_) {
            val_.some.~value_type();
        }
        else {
            val_.none.~None();
        }
        it_.~IteratorType();
        // std::cout << "destructed" << std::endl;
    }

private:
    //! TODO put in separate optional implementation (eckit::Optional is doing mem
    //! allocation...)
    struct None {};
    union opt_value_type {
        None none;
        value_type some;
        ~opt_value_type(){};
    };
    template<typename ItType>
    IteratorMapper(ForwardItContainer const& container, Mapper const& mapper, ItType&& it,
                   const value_type& val) :
        container_(container),
        mapper_(mapper),
        hasValue_(true),
        it_(std::forward<IteratorType>(it)),
        val_{.none = None{}} {
            // Always use new (&val_.some) to assign a new value. Using the = assignment would cause a segfault for non-trivial classes
            new (&val_.some) value_type(val);
    }
        
    template<typename ItType>
    IteratorMapper(ForwardItContainer const& container, Mapper const& mapper, ItType&& it,
                   value_type&& val) :
        container_(container),
        mapper_(mapper),
        hasValue_(true),
        it_(std::forward<ItType>(it)),
        val_{.none = None{}} {
            // Always use new (&val_.some) to assign a new value. Using the = assignment would cause a segfault for non-trivial classes
            new (&val_.some) value_type(std::move(val));
    }

    template<typename ItType>
    IteratorMapper(ForwardItContainer const& container, Mapper const& mapper, bool hasValue,
                   ItType&& it) :
        container_(container),
        mapper_(mapper),
        hasValue_(hasValue),
        it_(std::forward<ItType>(it)),
        val_{.none = None{}} {
        // std::cout << "I may fail here; hasValue " << hasValue_ << std::endl;
        if (hasValue_) {
           // std::cout << "Mapped value" << mapper(*it_).config() << std::endl;
            // Always use new (&val_.some) to assign a new value. Using the = assignment would cause a segfault for non-trivial classes
            new (&val_.some) value_type(mapper(*it_));
        }
        // std::cout << "Wow initialized" << std::endl;
    }
   
  
    template<typename ItType>
    IteratorMapper(ForwardItContainer const& container, Mapper const& mapper, ItType&& it) :
        IteratorMapper(container, mapper, it != container.cend(), std::forward<ItType>(it)) {}

    IteratorMapper(ForwardItContainer const& container, Mapper const& mapper) :
        IteratorMapper(container, mapper, container.cbegin()) {}


    ForwardItContainer const& container_;
    Mapper const& mapper_;

    bool hasValue_;
    IteratorType it_;
    opt_value_type val_;

    friend class MappedContainer<ForwardItContainer, Mapper>;
};

template <typename ForwardItContainer, class Mapper>
class MappedContainer {
public:
    using iterator = IteratorMapper<ForwardItContainer, Mapper, false>;
    using const_iterator = IteratorMapper<ForwardItContainer, Mapper, true>;

    using This = MappedContainer<ForwardItContainer, Mapper>;

    template<typename Cont_, typename Mapper_>
    MappedContainer(Cont_&& container, Mapper_&& mapper) :
        container_(std::forward<Cont_>(container)),
        mapper_(std::forward<Mapper_>(mapper)) {}


    iterator begin() const { 
        // std::cout << "MappedContainer::begin" << std::endl; 
        return iterator(container_, mapper_); 
    }
    iterator end() const { 
        // std::cout << "MappedContainer::end" << std::endl; 
        return iterator(container_, mapper_, false, container_.end()); 
    }

    iterator cbegin() const { 
        // std::cout << "MappedContainer::cbegin" << std::endl; 
        return const_iterator(container_, mapper_); 
    }
    iterator cend() const { 
        // std::cout << "MappedContainer::cend" << std::endl; 
        return const_iterator(container_, mapper_, false, container_.end()); 
    }

private:
    ForwardItContainer container_;
    Mapper mapper_;
};

}  // namespace util
}  // namespace multio

#endif
