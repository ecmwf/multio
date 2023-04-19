
#include "multio/util/Environment.h"

#include <cstring>

namespace multio {
namespace util {

std::optional<std::string_view> getEnv(const char* var) {
    char* varVal = std::getenv(var);
    return (varVal != NULL && (std::strlen(varVal) > 0)) ? std::optional<std::string_view>{varVal}
                                                         : std::optional<std::string_view>{};
}

std::optional<std::string_view> getEnv(const std::string& var) {
    return getEnv(var.c_str());
}

std::optional<std::string_view> getEnv(std::string_view var) {
    std::string tmpVar{var};
    return getEnv(tmpVar.c_str());
}

}  // namespace util
}  // namespace multio
