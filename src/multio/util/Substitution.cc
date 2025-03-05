#include "Substitution.h"

#include "eckit/utils/Translator.h"
#include "eckit/value/Value.h"


namespace multio::util {

std::optional<bool> parseBool(const eckit::LocalConfiguration& cfg, const std::string& key, bool defaultValue) {

    // return cfg.has(key) 
    //     ? (cfg.isString(key)
    //         ? (std::cout << "String branch selected. Key: " << key 
    //                     << ", Value: " << cfg.getString(key) << std::endl,
    //         std::optional<bool>{eckit::Translator<std::string, bool>{}(util::replaceCurly(
    //             cfg.getString(key),
    //             [](std::string_view replace) {
    //                 std::string lookUpKey{replace};
    //                 char* env = ::getenv(lookUpKey.c_str());
    //                 return env ? std::optional<std::string>{env} : std::optional<std::string>{};
    //             }))}
    //         )
    //         : (cfg.isBoolean(key)
    //             ? (std::cout << "Boolean branch selected. Key: " << key 
    //                         << ", Value: " << cfg.getBool(key) << std::endl,
    //             std::optional<bool>{cfg.getBool(key)})
    //             : (std::cout << "Default value branch selected." << std::endl,
    //             std::optional<bool>{}))
    //     )
    //     : (std::cout << "Default value branch selected." << std::endl,
    //     std::optional<bool>{defaultValue});


    
    return cfg.has(key) ? cfg.isString(key)
                            ? std::optional<bool>{eckit::Translator<std::string, bool>{}(util::replaceCurly(
                                  cfg.getString(key),
                                  [](std::string_view replace) {
                                      std::string lookUpKey{replace};
                                      char* env = ::getenv(lookUpKey.c_str());
                                      return env ? std::optional<std::string>{env} : std::optional<std::string>{};
                                  }))}
                            : (cfg.isBoolean(key) ? std::optional<bool>{cfg.getBool(key)} : std::optional<bool>{})
                       : std::optional<bool>{defaultValue};
}

std::optional<bool> parseEnabled(const eckit::LocalConfiguration& cfg, bool defaultValue) {
    return parseBool(cfg, "enable", defaultValue);
}

}  // namespace multio::util
