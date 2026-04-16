#include "Parser.h"

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
        throw eckit::UserError("Cannot convert '" + str + "' to int64 : input is not numeric", Here());
    }
    catch (const std::out_of_range& e) {
        throw eckit::UserError("Cannot convert '" + str + "' to int64 : output value is out of range", Here());
    }
    if (pos != str.size()) {
        throw eckit::UserError("Cannot convert '" + str + "' to int64 : input contains trailing characters", Here());
    }
}

void parseDoubleStrict(double& value, const std::string& str) {
    size_t pos;
    try {
        value = std::stod(str, &pos);
    }
    catch (const std::invalid_argument& e) {
        throw eckit::UserError("Cannot convert '" + str + "' to double : input is not numeric", Here());
    }
    catch (const std::out_of_range& e) {
        throw eckit::UserError("Cannot convert '" + str + "' to double : output value is out of range", Here());
    }
    if (pos != str.size()) {
        throw eckit::UserError("Cannot convert '" + str + "' to double : input contains trailing characters", Here());
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


bool parseEntry(eckit::LocalConfiguration& value, const std::string& key,
                const eckit::LocalConfiguration& localConfig) {
    if (!localConfig.has(key)) {
        return false;
    }
    if (localConfig.isSubConfiguration(key)) {
        value = localConfig.getSubConfiguration(key);
        return true;
    }
    throw eckit::UserError{
        "Could not convert value of key '" + key + "' to eckit::LocalConfiguration: no conversion method defined",
        Here()};
}

bool parseEntry(std::vector<double>& value, const std::string& key, const eckit::LocalConfiguration& localConfig) {
    if (!localConfig.has(key)) {
        return false;
    }
    if (localConfig.isFloatingPointList(key)) {
        value = localConfig.getDoubleVector(key);
        return true;
    }
    throw eckit::UserError{
        "Could not convert value of key '" + key + "' to vector<double>: no conversion method defined", Here()};
}

bool parseEntry(std::vector<std::string>& value, const std::string& key, const eckit::LocalConfiguration& localConfig) {
    if (!localConfig.has(key)) {
        return false;
    }
    if (localConfig.isStringList(key)) {
        value = localConfig.getStringVector(key);
        return true;
    }
    throw eckit::UserError{
        "Could not convert value of key '" + key + "' to vector<int64_t>: no conversion method defined", Here()};
}
bool parseEntry(std::vector<std::int64_t>& value, const std::string& key,
                const eckit::LocalConfiguration& localConfig) {
    if (!localConfig.has(key)) {
        return false;
    }
    if (localConfig.isIntegralList(key)) {
        auto res = localConfig.getLongVector(key);
        value = std::vector<std::int64_t>{res.begin(), res.end()};
        return true;
    }
    throw eckit::UserError{
        "Could not convert value of key '" + key + "' to vector<int64_t>: no conversion method defined", Here()};
}

}  // namespace multio::util::config::detail
