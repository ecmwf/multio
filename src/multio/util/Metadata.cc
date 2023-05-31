#include "Metadata.h"  // Include header now

#include "eckit/config/Configuration.h"
#include "eckit/config/LocalConfiguration.h"

#include <optional>

namespace multio::util {

std::optional<long> lookUpLong(const eckit::Configuration& c, const std::string& key) {
    return c.has(key) ? std::optional<long>{c.getLong(key)} : std::optional<long>{};
}

std::optional<std::string> lookUpString(const eckit::Configuration& c, const std::string& key) {
    return c.has(key) ? std::optional<std::string>{c.getString(key)} : std::optional<std::string>{};
}

std::optional<double> lookUpDouble(const eckit::Configuration& c, const std::string& key) {
    return c.has(key) ? std::optional<double>{c.getDouble(key)} : std::optional<double>{};
}

std::optional<float> lookUpFloat(const eckit::Configuration& c, const std::string& key) {
    return c.has(key) ? std::optional<float>{c.getFloat(key)} : std::optional<float>{};
}

std::optional<bool> lookUpBool(const eckit::Configuration& c, const std::string& key) {
    return c.has(key) ? std::optional<bool>{c.getBool(key)} : std::optional<bool>{};
}


template <>
std::optional<long> LookUp<long>::operator()() const {
    return lookUpLong(c_, key_);
}
template <>
std::optional<std::string> LookUp<std::string>::operator()() const {
    return lookUpString(c_, key_);
}
template <>
std::optional<double> LookUp<double>::operator()() const {
    return lookUpDouble(c_, key_);
}
template <>
std::optional<float> LookUp<float>::operator()() const {
    return lookUpFloat(c_, key_);
}
template <>
std::optional<bool> LookUp<bool>::operator()() const {
    return lookUpBool(c_, key_);
}


//-----------------------------------------------------------------------------------------------------------------------------------------


}  // namespace multio::util
