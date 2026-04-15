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
#include "multio/message/MetadataException.h"


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
    const std::string k{key};

    // Use untyped getOpt to safely inspect the MetadataValue variant without triggering
    // std::terminate (the typed getOpt<T> is incorrectly marked noexcept but throws on type mismatch).
    auto opt = md.getOpt(k);
    if (!opt.has_value()) {
        return false;  // Key not found
    }

    auto& mv = *opt;
    // Try extracting as int64 directly from the variant
    bool parsed = mv.visit([&](const auto& v) -> bool {
        using T = std::decay_t<decltype(v)>;
        if constexpr (std::is_same_v<T, std::int64_t>) {
            value = v;
            return true;
        }
        else if constexpr (std::is_same_v<T, std::string>) {
            // Parse numeric string, possibly with time-duration suffix
            try {
                size_t pos;
                auto num = std::stol(v, &pos);
                if (pos == v.size()) {
                    value = num;
                    return true;
                }
                // Handle "Xh" (hours) and "Xs" (seconds→hours) suffixes
                if (pos + 1 == v.size()) {
                    char suffix = v[pos];
                    if (suffix == 'h') {
                        value = num;
                        return true;
                    }
                    if (suffix == 's') {
                        value = num / 3600;
                        return true;
                    }
                }
            }
            catch (const std::invalid_argument&) {
            }
            catch (const std::out_of_range&) {
            }
            return false;  // Conversion failed — will throw below
        }
        else if constexpr (std::is_same_v<T, double>) {
            value = static_cast<std::int64_t>(v);
            return true;
        }
        else if constexpr (std::is_same_v<T, bool>) {
            value = v ? 1 : 0;
            return true;
        }
        else {
            return false;
        }
    });

    if (!parsed) {
        throw eckit::UserError("Cannot convert metadata value of key '" + k + "' to int64", Here());
    }
    return true;
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
