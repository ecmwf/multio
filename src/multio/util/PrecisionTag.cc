#include "multio/util/PrecisionTag.h"

namespace multio::util {

/*
size_t decodePrecisionSize(const std::string& tagStr) {
    const static std::map<std::string, size_t> str2size{{"single", size_t(sizeof(float))},
                                                        {"double", size_t(sizeof(double))}};
    return str2size.at(tagStr);
}
*/

PrecisionTag decodePrecisionTag(const std::string& tagStr) {
    const static std::map<std::string, PrecisionTag> str2tag{{"single", PrecisionTag::Float},
                                                             {"double", PrecisionTag::Double},
                                                             {"misc-single", PrecisionTag::Float},
                                                             {"misc-double", PrecisionTag::Double}};
    return str2tag.at(tagStr);
}

}  // namespace multio::util
