
#pragma once

#include <map>
#include <string>
#include <iostream>
#include <sstream>

#include "eckit/exception/Exceptions.h"

namespace multio {
namespace util {

enum class PrecisionTag : unsigned
{
    Float = 1,
    Double = 2,
};


template <typename T>
struct PrecisionType {
    using type = T;
};

// C++14
template <typename T>
using PrecisionType_t = typename PrecisionType<T>::type;

// size_t decodePrecisionSize(const std::string& tagStr);
PrecisionTag decodePrecisionTag(const std::string& tagStr);


template <typename Func>
decltype(auto) dispatchPrecisionTag(const std::string& tagStr, Func&& f) {
    // may throw
    return dispatchPrecisionTag(decodePrecisionTag(tagStr), std::forward<Func>(f));
}

// Disptach function might not return a value...
// Decltype(auto) requires at least C++14... moreover the returntype must be the same for all dispatching results. If
// not std::variant may be used...
template <typename Func>
decltype(auto) dispatchPrecisionTag(PrecisionTag t, Func&& f) {
    switch (t) {
        case PrecisionTag::Float: {
            return std::forward<Func>(f)(PrecisionType<float>{});
        }
        case PrecisionTag::Double: {
            return std::forward<Func>(f)(PrecisionType<double>{});
        }
        default:
            std::ostringstream oss;
            oss << "Error in dispatchPrecisionTag: Unkown tag " << ((unsigned) t) << std::endl;;
            throw eckit::SeriousBug(oss.str(), Here());
    }
}


}  // namespace util
}  // namespace multio
