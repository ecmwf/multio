#include "multio/util/PrecisionTag.h"

namespace multio {
namespace util {

PrecisionTag decodePrecisionTag(const std::string& tagStr) {
    const static std::map<std::string, PrecisionTag> str2tag{{"single", PrecisionTag::Float},
                                                             {"double", PrecisionTag::Double}};
    return str2tag.at(tagStr);
}

}  // namespace util
}  // namespace multio