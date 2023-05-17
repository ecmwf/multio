#include "Substitution.h"

#include "eckit/utils/Translator.h"
#include "eckit/value/Value.h"


namespace multio::util {

std::optional<bool> parseBool(const eckit::LocalConfiguration& cfg, const std::string& key, bool defaultValue) {

    eckit::Value input = cfg.has(key)
                           ? cfg.getSubConfiguration(key).get().isString()
                               ? eckit::Value{eckit::Translator<std::string, bool>{}(util::replaceCurly(
                                   cfg.getString(key),
                                   [](std::string_view replace) {
                                       std::string lookUpKey{replace};
                                       char* env = ::getenv(lookUpKey.c_str());
                                       return env ? std::optional<std::string>{env} : std::optional<std::string>{};
                                   }))}
                               : cfg.getSubConfiguration(key).get()
                           : eckit::Value{defaultValue};
    return input.isBool() ? std::optional<bool>{input.as<bool>()} : std::optional<bool>{};
}

std::optional<bool> parseEnabled(const eckit::LocalConfiguration& cfg, bool defaultValue) {
    return parseBool(cfg, "enable", defaultValue);
}

}  // namespace multio::util
