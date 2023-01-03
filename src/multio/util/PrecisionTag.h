
#pragma once

#include <map>
#include <string>

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


PrecisionTag decodePrecisionTag(const std::string& tagStr);


// TODO: Need specializations
template <typename Func>
decltype(auto) dispatchPrecisionTag(const std::string& tagStr, Func&& f);

// Disptach function might not return a value...
// Decltype(auto) requires at least C++14... moreover the returntype must be the same for all dispatching results. If
// not std::variant may be used...
template <typename Func>
decltype(auto) dispatchPrecisionTag(PrecisionTag t, Func&& f);


}  // namespace util
}  // namespace multio
