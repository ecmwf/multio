
#pragma once

#include <optional>
#include <string>
#include <string_view>

namespace multio::util {

std::optional<std::string_view> getEnv(std::string_view);
std::optional<std::string_view> getEnv(const char*);
std::optional<std::string_view> getEnv(const std::string&);


}  // namespace multio::util
