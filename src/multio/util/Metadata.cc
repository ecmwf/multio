#include "Metadata.h"  // Include header now

#include "eckit/config/Configuration.h"
#include "eckit/config/LocalConfiguration.h"

#include <optional>

namespace multio::util {

std::optional<std::int64_t> lookUpInt64(const eckit::Configuration& c, const std::string& key) {
    return c.has(key) ? std::optional<std::int64_t>{c.getInt64(key)} : std::optional<std::int64_t>{};
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


//-----------------------------------------------------------------------------------------------------------------------------------------


}  // namespace multio::util
