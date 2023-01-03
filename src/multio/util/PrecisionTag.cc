#include "multio/util/PrecisionTag.h"

namespace multio {
namespace util {

PrecisionTag decodePrecisionTag(const std::string& tagStr) {
    const static std::map<std::string, PrecisionTag> str2tag{{"single", PrecisionTag::Float},
                                                             {"double", PrecisionTag::Double}};
    return str2tag.at(tagStr);
}

template <typename Func>
decltype(auto) dispatchPrecisionTag(const std::string& tagStr, Func&& f) {
    // may throw
    return dispatchPrecisionTag(decodePrecisionTag(tagStr), std::forward<Func>(f));
}

template <typename Func>
decltype(auto) dispatchPrecisionTag(PrecisionTag t, Func&& f) {
    switch (t) {
        case PrecisionTag::Float: {
            return std::forward<Func>(f)(PrecisionType<float>{});
        }
        case PrecisionTag::Double: {
            return std::forward<Func>(f)(PrecisionType<double>{});
        }
    }
}

}  // namespace util
}  // namespace multio