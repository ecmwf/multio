#include "Substitution.h"

#include "eckit/value/Value.h"
#include "eckit/utils/Translator.h"

namespace multio {
namespace util {

eckit::Optional<bool> parseEnabled( const eckit::LocalConfiguration& cfg, bool defaultValue ){

    eckit::Value input = cfg.has("enable") ? 
        cfg.getSubConfiguration("enable").get().isString() ? 
            eckit::Value{ 
                eckit::Translator<std::string,bool>{}(util::replaceCurly(
                    cfg.getString("enable"), 
                    [ ](std::string_view replace) {
                        std::string lookUpKey{replace};
                        char* env = ::getenv(lookUpKey.c_str());
                        return env ? eckit::Optional<std::string>{env} : eckit::Optional<std::string>{};
                    }
                )) 
            } : cfg.getSubConfiguration("enable").get() 
        : eckit::Value{defaultValue};
    return input.isBool() ? eckit::Optional<bool>{input.as<bool>()} : eckit::Optional<bool>{};
};

}}