#include "Substitution.h"

namespace multio {
namespace util {

std::string replaceCurly(std::string_view s, const eckit::Configuration& replacements) {
    return replaceCurly(s, [&](std::string_view key) {
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
