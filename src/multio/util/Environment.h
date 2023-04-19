
#pragma once

#include <string_view>
#include <string>
#include <optional>

namespace multio {
namespace util {

std::optional<std::string_view> getEnv(std::string_view);
std::optional<std::string_view> getEnv(const char*);
std::optional<std::string_view> getEnv(const std::string&);


}  // namespace util
}  // namespace multio
