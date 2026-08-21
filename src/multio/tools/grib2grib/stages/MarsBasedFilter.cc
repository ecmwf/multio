/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/// @file
/// @brief Standalone `MarsBasedFilter` stage implementation for the isolated `grib2grib` pipeline.

#include "multio/tools/grib2grib/stages/MarsBasedFilter.h"

#include <cctype>
#include <limits>
#include <unordered_set>

#include "eckit/exception/Exceptions.h"

#include "multio/message/Metadata.h"

namespace multio::distGrib1ToGrib2::grib2grib {

namespace {

namespace detail {

bool isAccumulation(long param) {
    static const std::unordered_set<long> params{
        8,      9,      20,     44,     45,     47,     50,     57,     58,     169,    189,    205,    228,    239,
        240,    3062,   3099,   228021, 228022, 228129, 228130, 228143, 228144, 228216, 228228, 228251, 231005, 231010,
        231012, 231057, 231058, 260259, 131060, 131061, 131062, 131063, 131064, 131085, 131098, 131099,
    };

    return params.count(param) != 0 || (param >= 142 && param <= 147) || (param >= 175 && param <= 182)
        || (param >= 195 && param <= 197) || (param >= 208 && param <= 213) || (param >= 162100 && param <= 162113)
        || (param >= 222001 && param <= 222256) || (param >= 231001 && param <= 231003)
        || (param >= 233000 && param <= 233035) || (param >= 228080 && param <= 228082)
        || (param >= 235062 && param <= 235064);
}

bool isAllowedExtendedOperation(long param) {
    static const std::unordered_set<long> params{
        49,     121,    122,    123,    201,    202,    131065, 131066, 131067, 131070, 131071, 131072, 131089, 131090,
        131091, 131100, 141208, 141209, 141215, 141216, 141220, 141229, 141231, 141232, 141233, 141245, 143208, 143209,
        143215, 143216, 143220, 143229, 143231, 143232, 143233, 143245, 144208, 144209, 144215, 144216, 144220, 144229,
        144231, 144232, 144233, 144245, 228004, 228005, 228026, 228027, 228028, 228035, 228036, 228051, 228053, 228222,
        228223, 228224, 228225, 228226, 228227, 235020, 235021, 235055, 235058, 235083, 235084, 235087, 235088, 235090,
        235091, 235093, 235094, 235097, 235098, 235100, 235108, 235117, 235151, 235152, 235155, 235157, 235159, 235165,
        235166, 235168, 235189, 235203, 235246, 235263, 235269, 235283, 235287, 235288, 235290, 235305, 235309, 235320,
        235322, 235326, 235339, 235383, 237013, 237041, 237042, 237055, 237077, 237078, 237079, 237080, 237083, 237084,
        237087, 237088, 237090, 237091, 237093, 237094, 237097, 237108, 237117, 237131, 237132, 237134, 237137, 237151,
        237159, 237203, 237207, 237263, 237287, 237288, 237290, 237305, 237309, 237318, 237320, 237321, 237322, 237326,
        238013, 238041, 238042, 238055, 238077, 238078, 238079, 238080, 238083, 238084, 238087, 238088, 238090, 238091,
        238093, 238094, 238097, 238108, 238117, 238131, 238132, 238134, 238137, 238151, 238159, 238203, 238207, 238263,
        238287, 238288, 238290, 238305, 238309, 238320, 238322, 238326, 263024, 263107, 265024, 266024,
    };

    return params.count(param) != 0 || (param >= 141101 && param <= 141105) || (param >= 143101 && param <= 143105)
        || (param >= 144101 && param <= 144105) || (param >= 228057 && param <= 228060)
        || (param >= 235029 && param <= 235031) || (param >= 235033 && param <= 235043)
        || (param >= 235048 && param <= 235053) || (param >= 235077 && param <= 235080)
        || (param >= 235129 && param <= 235138) || (param >= 237165 && param <= 237168)
        || (param >= 238165 && param <= 238168);
}

bool isForecast(const std::string& type) {
    static const std::unordered_set<std::string> types{
        "fc",  "cf",  "pf",     "cm",  "fp",   "em",   "ep",  "es",  "fa",  "efi",    "efic",  "bf",    "cd",
        "wem", "wes", "cr",     "ses", "taem", "taes", "sg",  "sf",  "if",  "fcmean", "fcmax", "fcmin", "fcstdev",
        "ssd", "tf",  "hcmean", "s3",  "si",   "gbf",  "gwt", "est", "icp", "pfc",    "sot",   "4v",
    };
    return types.count(type) != 0;
}

bool isAssimilationStart(const std::string& type) {
    return type == "4i" || type == "me" || type == "eme";
}

long durationStringToSeconds(const std::string& raw) {
    if (raw.empty()) {
        throw eckit::BadValue("Empty duration", Here());
    }

    std::string value = raw;
    for (char& c : value) {
        c = static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
    }

    char unit = 'h';
    if (std::isalpha(static_cast<unsigned char>(value.back()))) {
        unit = value.back();
        value.pop_back();
    }

    std::size_t parsed = 0;
    const long count = std::stol(value, &parsed);
    if (parsed != value.size()) {
        throw eckit::BadValue("Invalid duration: " + raw, Here());
    }

    long multiplier = 0;
    switch (unit) {
        case 'h':
            multiplier = 3600;
            break;
        case 'm':
            multiplier = 60;
            break;
        case 's':
            multiplier = 1;
            break;
        case 'd':
            multiplier = 86400;
            break;
        default:
            throw eckit::BadValue("Unsupported duration unit: " + raw, Here());
    }

    if (count > std::numeric_limits<long>::max() / multiplier
        || count < std::numeric_limits<long>::min() / multiplier) {
        throw eckit::BadValue("Duration overflow: " + raw, Here());
    }
    return count * multiplier;
}

long durationToSeconds(const eckit::LocalConfiguration& mars, const std::string& key) {
    if (mars.isIntegral(key)) {
        const long hours = mars.getLong(key);
        if (hours > std::numeric_limits<long>::max() / 3600 || hours < std::numeric_limits<long>::min() / 3600) {
            throw eckit::BadValue("Duration overflow for '" + key + "'", Here());
        }
        return hours * 3600;
    }
    return durationStringToSeconds(mars.getString(key));
}

}  // namespace detail

bool parseApiOption(const eckit::LocalConfiguration& config, const std::string& option, bool defaultValue = false) {
    if (!config.has("api-options")) {
        return defaultValue;
    }

    if (!config.isSubConfiguration("api-options")) {
        throw eckit::BadValue("mars-based-filter option 'api-options' must be a configuration section", Here());
    }

    const auto apiOptions = config.getSubConfiguration("api-options");
    return apiOptions.has(option) ? apiOptions.getBool(option) : defaultValue;
}

bool rejectFromStartStatisticsForAnalysis(const eckit::LocalConfiguration& mars,
                                          const MarsBasedFilterContext& context) {
    if (context.allowFromStartStatisticsForAnalysis || !mars.has("type") || !mars.has("timespan")
        || mars.getString("type") != "an" || !mars.isString("timespan")) {
        return false;
    }

    std::string timespan = mars.getString("timespan");
    for (char& c : timespan) {
        c = static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
    }
    return timespan == "fs" || timespan == "from-start" || timespan == "fromstart";
}

bool rejectPartialStatisticsWindow(const eckit::LocalConfiguration& mars, const MarsBasedFilterContext& context) {
    if (context.allowPartialStatisticsWindow || !mars.has("type") || !mars.has("step") || !mars.has("timespan")) {
        return false;
    }

    const auto type = mars.getString("type");
    if (!detail::isForecast(type) && !detail::isAssimilationStart(type)) {
        return false;
    }

    if (mars.isString("timespan")) {
        std::string timespan = mars.getString("timespan");
        for (char& c : timespan) {
            c = static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
        }
        if (timespan == "none" || timespan == "fs" || timespan == "from-start" || timespan == "fromstart") {
            return false;
        }
    }

    return detail::durationToSeconds(mars, "step") < detail::durationToSeconds(mars, "timespan");
}

bool isStepZero(const eckit::LocalConfiguration& mars) {
    if (!mars.has("step")) {
        return false;
    }
    if (mars.isIntegral("step")) {
        return mars.getLong("step") == 0;
    }
    return mars.getString("step") == "0";
}

bool rejectUnsupportedStatisticalProcessingAtStepZero(const eckit::LocalConfiguration& mars,
                                                      const MarsBasedFilterContext& context) {
    if (!isStepZero(mars) || (!mars.has("timespan") && !mars.has("stattype"))) {
        return false;
    }

    const auto param = mars.getLong("param");
    if (detail::isAccumulation(param)) {
        return false;
    }

    if (context.allowExtendedSetOfOperationsForZeroLengthFsWindow && detail::isAllowedExtendedOperation(param)) {
        return false;
    }

    return true;
}

std::optional<multio::message::match::MatchReduce> parseSelectors(const eckit::LocalConfiguration& config) {
    if (!config.has("selectors")) {
        return std::nullopt;
    }

    if (!config.isSubConfiguration("selectors")) {
        throw eckit::BadValue("mars-based-filter option 'selectors' must be a configuration section", Here());
    }

    return multio::message::match::MatchReduce::construct(config.getSubConfiguration("selectors"));
}

multio::message::Metadata buildMetadata(const eckit::LocalConfiguration& mars, const eckit::LocalConfiguration& misc) {
    multio::message::Metadata metadata = multio::message::toMetadata(mars);
    metadata.updateOverwrite(multio::message::toMetadata(misc));
    return metadata;
}

}  // namespace

void validateMarsBasedFilterContext(const eckit::LocalConfiguration& config) {
    if (config.has("verbosity")) {
        (void)config.getLong("verbosity");
    }

    (void)parseApiOption(config, "allowExtendedSetOfOperationsForZeroLengthFsWindow");
    (void)parseApiOption(config, "allowFromStartStatisticsForAnalysis");
    (void)parseApiOption(config, "allowPartialStatisticsWindow", true);
    (void)parseSelectors(config);
}

MarsBasedFilterContext parseMarsBasedFilterContext(const eckit::LocalConfiguration& config) {
    MarsBasedFilterContext parsed;

    parsed.verbosity = config.has("verbosity") ? config.getLong("verbosity") : 0;
    if (parsed.verbosity < 0) {
        parsed.verbosity = 0;
    }
    if (parsed.verbosity > 3) {
        parsed.verbosity = 3;
    }

    parsed.allowExtendedSetOfOperationsForZeroLengthFsWindow
        = parseApiOption(config, "allowExtendedSetOfOperationsForZeroLengthFsWindow");
    parsed.allowFromStartStatisticsForAnalysis = parseApiOption(config, "allowFromStartStatisticsForAnalysis");
    parsed.allowPartialStatisticsWindow = parseApiOption(config, "allowPartialStatisticsWindow", true);
    parsed.selectors = parseSelectors(config);

    return parsed;
}

void freeMarsBasedFilterContext(MarsBasedFilterContext& context) noexcept {
    (void)context;
}

MarsBasedFilterCode runMarsBasedFilterStage(const eckit::LocalConfiguration& mars,
                                            const eckit::LocalConfiguration& misc,
                                            const MarsBasedFilterContext& context) noexcept {
    try {
        if (rejectFromStartStatisticsForAnalysis(mars, context)) {
            return MarsBasedFilterCode::Rejected;
        }

        if (rejectPartialStatisticsWindow(mars, context)) {
            return MarsBasedFilterCode::Rejected;
        }

        if (rejectUnsupportedStatisticalProcessingAtStepZero(mars, context)) {
            return MarsBasedFilterCode::Rejected;
        }

        if (!context.selectors) {
            return MarsBasedFilterCode::Accepted;
        }

        const multio::message::Metadata metadata = buildMetadata(mars, misc);
        return context.selectors->matches(metadata) ? MarsBasedFilterCode::Rejected : MarsBasedFilterCode::Accepted;
    }
    catch (...) {
        return MarsBasedFilterCode::Rejected;
    }
}

}  // namespace multio::distGrib1ToGrib2::grib2grib
