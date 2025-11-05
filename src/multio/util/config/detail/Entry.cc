#include "Entry.h"

#include <cstdint>
#include <stdexcept>
#include <string>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/log/CodeLocation.h"

namespace multio::util::config::detail {


void parseIntStrict(std::int64_t& value, const std::string& str) {
    size_t pos;
    try {
        value = std::stol(str, &pos);
    }
    catch (const std::invalid_argument& e) {
        throw eckit::UserError("Cannot convert '" + str + "' to int64 : input is not numeric");
    }
    catch (const std::out_of_range& e) {
        throw eckit::UserError("Cannot convert '" + str + "' to int64 : output value is out of range");
    }
    if (pos != str.size()) {
        throw eckit::UserError("Cannot convert '" + str + "' to int64 : input contains trailing characters");
    }
}

void parseDoubleStrict(double& value, const std::string& str) {
    size_t pos;
    try {
        value = std::stod(str, &pos);
    }
    catch (const std::invalid_argument& e) {
        throw eckit::UserError("Cannot convert '" + str + "' to double : input is not numeric");
    }
    catch (const std::out_of_range& e) {
        throw eckit::UserError("Cannot convert '" + str + "' to double : output value is out of range");
    }
    if (pos != str.size()) {
        throw eckit::UserError("Cannot convert '" + str + "' to double : input contains trailing characters");
    }
}

bool parseEntry(std::string& value, const std::string& key, const eckit::LocalConfiguration& localConfig) {
    if (!localConfig.has(key)) {
        return false;
    }
    if (localConfig.isString(key)) {
        value = localConfig.getString(key);
        return true;
    }
    throw eckit::UserError{"Could not convert value of key '" + key + "' to string : no conversion method defined",
                           Here()};
}

bool parseEntry(std::int64_t& value, const std::string& key, const eckit::LocalConfiguration& localConfig) {
    if (!localConfig.has(key)) {
        return false;
    }
    if (localConfig.isIntegral(key)) {
        value = localConfig.getInt64(key);
        return true;
    }
    if (localConfig.isString(key)) {
        parseIntStrict(value, localConfig.getString(key));
        return true;
    }
    throw eckit::UserError{"Could not convert value of key '" + key + "' to int64 : no conversion method defined",
                           Here()};
}

bool parseEntry(double& value, const std::string& key, const eckit::LocalConfiguration& localConfig) {
    if (!localConfig.has(key)) {
        return false;
    }
    if (localConfig.isFloatingPoint(key)) {
        value = localConfig.getDouble(key);
        return true;
    }
    if (localConfig.isIntegral(key)) {
        value = static_cast<double>(localConfig.getInt64(key));
        return true;
    }
    if (localConfig.isString(key)) {
        parseDoubleStrict(value, localConfig.getString(key));
        return true;
    }
    throw eckit::UserError{"Could not convert value of key '" + key + "' to double : no conversion method defined",
                           Here()};
}

bool parseEntry(bool& value, const std::string& key, const eckit::LocalConfiguration& localConfig) {
    if (!localConfig.has(key)) {
        return false;
    }
    if (localConfig.isBoolean(key)) {
        value = localConfig.getBool(key);
        return true;
    }
    if (localConfig.isIntegral(key)) {
        const int64_t configValue = localConfig.getInt64(key);
        if (configValue == 0) {
            value = false;
            return true;
        }
        if (configValue == 1) {
            value = true;
            return true;
        }
        throw eckit::UserError{"Integer value of boolean key '" + key + "' must be '1' or '0', given value was '"
                                   + std::to_string(configValue) + "'",
                               Here()};
    }
    if (localConfig.isString(key)) {
        const std::string& configValue = localConfig.getString(key);
        if (configValue == "0") {
            value = false;
            return true;
        }
        if (configValue == "1") {
            value = true;
            return true;
        }
        throw eckit::UserError{
            "String value of boolean key '" + key + "' must be '1' or '0', given value was '" + configValue + "'",
            Here()};
    }
    throw eckit::UserError{"Could not convert value of key '" + key + "' to double : no conversion method defined",
                           Here()};
}

}  // namespace multio::util::config::detail
