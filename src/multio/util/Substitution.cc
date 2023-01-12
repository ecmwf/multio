#include "Substitution.h"

namespace multio {
namespace util {

// Example
std::string replaceFish(std::string_view s, const std::map<std::string, std::string>& replacements) {
    return replaceFish(s, [&](std::string_view key) {
        auto const it = replacements.find(std::string{key});
        if (it != replacements.end()) {
            return std::optional<std::string_view>{it->second};
        }
        else {
            return std::optional<std::string_view>{};
        }
    });
}


// Example
std::string replaceFish(std::string_view s, const eckit::Configuration& replacements) {
    return replaceFish(s, [&](std::string_view key) {
        auto const k = std::string{key};
        if (replacements.has(k)) {
            return std::optional<std::string_view>{replacements.getString(k)};
        }
        else {
            return std::optional<std::string_view>{};
        }
    });
}

}  // namespace util
}  // namespace multio
