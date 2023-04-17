#include "Substitution.h"

#include "eckit/utils/Translator.h"
#include "eckit/value/Value.h"

namespace multio {
namespace util {


eckit::Optional<bool> parseBool(const eckit::LocalConfiguration& cfg, const std::string& key, bool defaultValue) {

    eckit::Value input = cfg.has(key)
                           ? cfg.getSubConfiguration(key).get().isString()
                               ? eckit::Value{eckit::Translator<std::string, bool>{}(util::replaceCurly(
                                   cfg.getString(key),
                                   [](std::string_view replace) {
                                       std::string lookUpKey{replace};
                                       char* env = ::getenv(lookUpKey.c_str());
                                       return env ? eckit::Optional<std::string>{env} : eckit::Optional<std::string>{};
                                   }))}
                               : cfg.getSubConfiguration(key).get()
                           : eckit::Value{defaultValue};
    return input.isBool() ? eckit::Optional<bool>{input.as<bool>()} : eckit::Optional<bool>{};
}

eckit::Optional<bool> parseEnabled(const eckit::LocalConfiguration& cfg, bool defaultValue) {
    return parseBool(cfg, "enable", defaultValue);
}

}  // namespace util
}  // namespace multio
