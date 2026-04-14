/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Parser.h"

#include <string>

#include "eckit/config/LocalConfiguration.h"

#include "multio/datamod/types/LevType.h"
#include "multio/datamod/types/Param.h"
#include "multio/datamod/types/StatType.h"
#include "multio/message/Metadata.h"


namespace multio::datamod::detail {


//----------------------------------------------------------------------------------------------------------------------
// Parse overloads -- read from Metadata
//----------------------------------------------------------------------------------------------------------------------

bool parseEntry(std::string& value, std::string_view key, const message::Metadata& md) {
    if (auto v = md.getOpt<std::string>(std::string(key))) {
        value = std::move(*v);
        return true;
    }
    return false;
}

bool parseEntry(std::int64_t& value, std::string_view key, const message::Metadata& md) {
    if (auto v = md.getOpt<std::int64_t>(std::string(key))) {
        value = *v;
        return true;
    }
    // Also accept string representation and parse
    if (auto v = md.getOpt<std::string>(std::string(key))) {
        try {
            size_t pos;
            value = std::stol(*v, &pos);
            if (pos == v->size()) {
                return true;
            }
        }
        catch (...) {
        }
        throw eckit::UserError("Cannot convert metadata value of key '" + std::string(key) + "' to int64: '" + *v + "'",
                               Here());
    }
    return false;
}

bool parseEntry(double& value, std::string_view key, const message::Metadata& md) {
    if (auto v = md.getOpt<double>(std::string(key))) {
        value = *v;
        return true;
    }
    // Accept integer as double
    if (auto v = md.getOpt<std::int64_t>(std::string(key))) {
        value = static_cast<double>(*v);
        return true;
    }
    return false;
}

bool parseEntry(bool& value, std::string_view key, const message::Metadata& md) {
    if (auto v = md.getOpt<bool>(std::string(key))) {
        value = *v;
        return true;
    }
    // Accept 0/1 as bool
    if (auto v = md.getOpt<std::int64_t>(std::string(key))) {
        if (*v == 0 || *v == 1) {
            value = (*v == 1);
            return true;
        }
        throw eckit::UserError("Integer value of boolean metadata key '" + std::string(key) + "' must be 0 or 1, got "
                                   + std::to_string(*v),
                               Here());
    }
    return false;
}

bool parseEntry(Param& value, std::string_view key, const message::Metadata& md) {
    // Try int64 first (most common representation)
    if (auto v = md.getOpt<std::int64_t>(std::string(key))) {
        value = Param(*v);
        return true;
    }
    // Try string (e.g., "2t", "tp")
    if (auto v = md.getOpt<std::string>(std::string(key))) {
        value = Param(*v);
        return true;
    }
    return false;
}

bool parseEntry(LevType& value, std::string_view key, const message::Metadata& md) {
    if (auto v = md.getOpt<std::string>(std::string(key))) {
        value = levTypeFromString(*v);
        return true;
    }
    return false;
}

bool parseEntry(StatType& value, std::string_view key, const message::Metadata& md) {
    if (auto v = md.getOpt<std::string>(std::string(key))) {
        value = StatType::fromString(*v);
        return true;
    }
    return false;
}

bool parseEntry(std::optional<StatType>& value, std::string_view key, const message::Metadata& md) {
    if (auto v = md.getOpt<std::string>(std::string(key))) {
        value = StatType::fromString(*v);
        return true;
    }
    return false;
}

bool parseEntry(std::vector<std::int64_t>& value, std::string_view key, const message::Metadata& md) {
    if (auto v = md.getOpt<std::vector<std::int64_t>>(std::string(key))) {
        value = std::move(*v);
        return true;
    }
    return false;
}

bool parseEntry(std::vector<double>& value, std::string_view key, const message::Metadata& md) {
    if (auto v = md.getOpt<std::vector<double>>(std::string(key))) {
        value = std::move(*v);
        return true;
    }
    return false;
}


//----------------------------------------------------------------------------------------------------------------------
// Dump overloads -- write to Metadata
//----------------------------------------------------------------------------------------------------------------------

void dumpEntry(const std::string& value, std::string_view key, message::Metadata& md) {
    md.set(std::string(key), value);
}

void dumpEntry(std::int64_t value, std::string_view key, message::Metadata& md) {
    md.set(std::string(key), value);
}

void dumpEntry(double value, std::string_view key, message::Metadata& md) {
    md.set(std::string(key), value);
}

void dumpEntry(bool value, std::string_view key, message::Metadata& md) {
    md.set(std::string(key), value);
}

void dumpEntry(const Param& value, std::string_view key, message::Metadata& md) {
    md.set(std::string(key), value.id());
}

void dumpEntry(LevType value, std::string_view key, message::Metadata& md) {
    md.set(std::string(key), levTypeToString(value));
}

void dumpEntry(const StatType& value, std::string_view key, message::Metadata& md) {
    md.set(std::string(key), value.toString());
}

void dumpEntry(const std::vector<std::int64_t>& value, std::string_view key, message::Metadata& md) {
    md.set(std::string(key), value);
}

void dumpEntry(const std::vector<double>& value, std::string_view key, message::Metadata& md) {
    md.set(std::string(key), value);
}


//----------------------------------------------------------------------------------------------------------------------
// Config dump overloads -- write to eckit::LocalConfiguration
//----------------------------------------------------------------------------------------------------------------------

void dumpConfigEntry(const std::string& value, const std::string& key, eckit::LocalConfiguration& conf) {
    conf.set(key, value);
}

void dumpConfigEntry(std::int64_t value, const std::string& key, eckit::LocalConfiguration& conf) {
    conf.set(key, value);
}

void dumpConfigEntry(double value, const std::string& key, eckit::LocalConfiguration& conf) {
    conf.set(key, value);
}

void dumpConfigEntry(bool value, const std::string& key, eckit::LocalConfiguration& conf) {
    conf.set(key, value);
}

void dumpConfigEntry(const Param& value, const std::string& key, eckit::LocalConfiguration& conf) {
    conf.set(key, value.id());
}

void dumpConfigEntry(LevType value, const std::string& key, eckit::LocalConfiguration& conf) {
    conf.set(key, levTypeToString(value));
}

void dumpConfigEntry(const StatType& value, const std::string& key, eckit::LocalConfiguration& conf) {
    conf.set(key, value.toString());
}

void dumpConfigEntry(const std::vector<std::int64_t>& value, const std::string& key, eckit::LocalConfiguration& conf) {
    conf.set(key, value);
}

void dumpConfigEntry(const std::vector<double>& value, const std::string& key, eckit::LocalConfiguration& conf) {
    conf.set(key, value);
}


}  // namespace multio::datamod::detail
