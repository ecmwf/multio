#include "multio/util/Substitution.h"

#include "multio/util/Environment.h"

#include "eckit/utils/Translator.h"
#include "eckit/value/Value.h"


namespace multio::util {

std::string replaceCurly(std::string_view s) {
    return replaceCurly(s, [](std::string_view replace) {
        std::string lookUpKey{replace};
        auto env = util::getEnv(lookUpKey);
        return env ? std::optional<std::string>{*env} : std::optional<std::string>{};
    });
}

std::optional<bool> parseBool(const eckit::LocalConfiguration& cfg, const std::string& key, bool defaultValue) {
    if (cfg.has(key)) {
        if (cfg.isString(key)) {
            return std::optional<bool>{eckit::Translator<std::string, bool>{}(cfg.getString(key))};
        }
        if (cfg.isBoolean(key)) {
            return std::optional<bool>{cfg.getBool(key)};
        }
        return std::optional<bool>{};
    }
    return std::optional<bool>{defaultValue};
}

std::optional<bool> parseEnabled(const eckit::LocalConfiguration& cfg, bool defaultValue) {
    return parseBool(cfg, "enable", defaultValue);
}

}  // namespace multio::util
