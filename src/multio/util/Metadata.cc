#include "Metadata.h"  // Include header now

#include "eckit/config/Configuration.h"
#include "eckit/config/LocalConfiguration.h"

namespace multio {
namespace util {

eckit::Optional<long> lookUpLong(const eckit::Configuration& c, const std::string& key) {
    return c.has(key) ? eckit::Optional<long>{c.getLong(key)} : eckit::Optional<long>{};
}

eckit::Optional<std::string> lookUpString(const eckit::Configuration& c, const std::string& key) {
    return c.has(key) ? eckit::Optional<std::string>{c.getString(key)} : eckit::Optional<std::string>{};
}

eckit::Optional<double> lookUpDouble(const eckit::Configuration& c, const std::string& key) {
    return c.has(key) ? eckit::Optional<double>{c.getDouble(key)} : eckit::Optional<double>{};
}

eckit::Optional<float> lookUpFloat(const eckit::Configuration& c, const std::string& key) {
    return c.has(key) ? eckit::Optional<float>{c.getFloat(key)} : eckit::Optional<float>{};
}

eckit::Optional<bool> lookUpBool(const eckit::Configuration& c, const std::string& key) {
    return c.has(key) ? eckit::Optional<bool>{c.getBool(key)} : eckit::Optional<bool>{};
}


template <>
eckit::Optional<long> LookUp<long>::operator()() const {
    return lookUpLong(c_, key_);
}
template <>
eckit::Optional<std::string> LookUp<std::string>::operator()() const {
    return lookUpString(c_, key_);
}
template <>
eckit::Optional<double> LookUp<double>::operator()() const {
    return lookUpDouble(c_, key_);
}
template <>
eckit::Optional<float> LookUp<float>::operator()() const {
    return lookUpFloat(c_, key_);
}
template <>
eckit::Optional<bool> LookUp<bool>::operator()() const {
    return lookUpBool(c_, key_);
}


//-----------------------------------------------------------------------------------------------------------------------------------------


}  // namespace util
}  // namespace multio
