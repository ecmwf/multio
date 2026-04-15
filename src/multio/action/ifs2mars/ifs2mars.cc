#include "multio/action/ifs2mars/detail/ifs2mars.h"

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <string>
#include <vector>

#include "eckit/exception/Exceptions.h"

namespace multio::action::ifs2mars::detail {
namespace {

using Metadata = message::Metadata;

constexpr std::uint64_t kHoursToSeconds = 3600ULL;
constexpr std::uint64_t kOriginEcmwf = 98ULL;

constexpr std::int64_t kPrefixModelLevel = 1;
constexpr std::int64_t kPrefixPressureLevel = 2;
constexpr std::int64_t kPrefixVorticityLevel = 3;
constexpr std::int64_t kPrefixThetaLevel = 4;
constexpr std::int64_t kPrefixSurface = 5;
constexpr std::int64_t kPrefixWaveInt = 6;
constexpr std::int64_t kPrefixWaveSpec = 7;
constexpr std::int64_t kPrefixHeightAboveGround = 8;
constexpr std::int64_t kPrefixAl = 9;

constexpr std::uint64_t kTypeOfStatisticalProcessInstant = 0ULL;
constexpr std::uint64_t kTypeOfStatisticalProcessAverage = 1ULL;
constexpr std::uint64_t kTypeOfStatisticalProcessAccumulation = 2ULL;
constexpr std::uint64_t kTypeOfStatisticalProcessMaximum = 3ULL;
constexpr std::uint64_t kTypeOfStatisticalProcessMinimum = 4ULL;

struct TimeProcInfo {
    bool valid = true;
    std::uint64_t secStart = 0;
    std::uint64_t secEnd = 0;
    std::uint64_t stepType = kTypeOfStatisticalProcessInstant;
};

struct Ymd {
    std::int64_t year = 0;
    std::int64_t month = 0;
    std::int64_t day = 0;
};

struct Hms {
    std::int64_t hour = 0;
    std::int64_t minute = 0;
    std::int64_t second = 0;
};

[[noreturn]] void fail(const std::string& msg) {
    throw eckit::SeriousBug("ifs2mars action: " + msg, Here());
}

void require(bool cond, const std::string& msg) {
    if (!cond) {
        fail(msg);
    }
}

template <typename T>
bool hasOpt(const Metadata& in, const std::string& key) {
    return static_cast<bool>(in.getOpt<T>(key));
}

bool hasKey(const Metadata& in, const std::string& key) {
    return hasOpt<std::string>(in, key) || hasOpt<bool>(in, key) || hasOpt<int>(in, key) || hasOpt<long>(in, key) ||
           hasOpt<std::int64_t>(in, key) || hasOpt<std::uint64_t>(in, key) || hasOpt<float>(in, key) ||
           hasOpt<double>(in, key) || hasOpt<std::vector<int>>(in, key) || hasOpt<std::vector<long>>(in, key) ||
           hasOpt<std::vector<std::int64_t>>(in, key) || hasOpt<std::vector<float>>(in, key) ||
           hasOpt<std::vector<double>>(in, key);
}

std::string getString(const Metadata& in, const std::string& key) {
    if (auto v = in.getOpt<std::string>(key)) {
        return *v;
    }
    fail("missing string key '" + key + "'");
}

bool getBool(const Metadata& in, const std::string& key) {
    if (auto v = in.getOpt<bool>(key)) {
        return *v;
    }
    fail("missing bool key '" + key + "'");
}

std::int64_t getInt(const Metadata& in, const std::string& key) {
    if (auto v = in.getOpt<std::int64_t>(key)) {
        return *v;
    }
    if (auto v = in.getOpt<long>(key)) {
        return static_cast<std::int64_t>(*v);
    }
    if (auto v = in.getOpt<int>(key)) {
        return static_cast<std::int64_t>(*v);
    }
    if (auto v = in.getOpt<std::uint64_t>(key)) {
        return static_cast<std::int64_t>(*v);
    }
    fail("missing integer key '" + key + "'");
}

std::uint64_t getUInt(const Metadata& in, const std::string& key) {
    if (auto v = in.getOpt<std::uint64_t>(key)) {
        return *v;
    }
    if (auto v = in.getOpt<std::int64_t>(key)) {
        require(*v >= 0, "key '" + key + "' must be non-negative");
        return static_cast<std::uint64_t>(*v);
    }
    if (auto v = in.getOpt<long>(key)) {
        require(*v >= 0, "key '" + key + "' must be non-negative");
        return static_cast<std::uint64_t>(*v);
    }
    if (auto v = in.getOpt<int>(key)) {
        require(*v >= 0, "key '" + key + "' must be non-negative");
        return static_cast<std::uint64_t>(*v);
    }
    fail("missing unsigned integer key '" + key + "'");
}

float getFloat(const Metadata& in, const std::string& key) {
    if (auto v = in.getOpt<float>(key)) {
        return *v;
    }
    if (auto v = in.getOpt<double>(key)) {
        return static_cast<float>(*v);
    }
    fail("missing float key '" + key + "'");
}

std::vector<std::int64_t> getIntArray(const Metadata& in, const std::string& key) {
    if (auto v = in.getOpt<std::vector<std::int64_t>>(key)) {
        return *v;
    }
    if (auto v = in.getOpt<std::vector<long>>(key)) {
        std::vector<std::int64_t> out;
        out.reserve(v->size());
        for (auto x : *v) {
            out.push_back(static_cast<std::int64_t>(x));
        }
        return out;
    }
    if (auto v = in.getOpt<std::vector<int>>(key)) {
        std::vector<std::int64_t> out;
        out.reserve(v->size());
        for (auto x : *v) {
            out.push_back(static_cast<std::int64_t>(x));
        }
        return out;
    }
    fail("missing integer array key '" + key + "'");
}

std::vector<float> getFloatArray(const Metadata& in, const std::string& key) {
    if (auto v = in.getOpt<std::vector<float>>(key)) {
        return *v;
    }
    if (auto v = in.getOpt<std::vector<double>>(key)) {
        std::vector<float> out;
        out.reserve(v->size());
        for (auto x : *v) {
            out.push_back(static_cast<float>(x));
        }
        return out;
    }
    fail("missing float array key '" + key + "'");
}

std::vector<double> getDoubleArray(const Metadata& in, const std::string& key) {
    if (auto v = in.getOpt<std::vector<double>>(key)) {
        return *v;
    }
    if (auto v = in.getOpt<std::vector<float>>(key)) {
        std::vector<double> out;
        out.reserve(v->size());
        for (auto x : *v) {
            out.push_back(static_cast<double>(x));
        }
        return out;
    }
    fail("missing double array key '" + key + "'");
}

void setString(Metadata& out, const std::string& key, const std::string& value) { out.set(key, value); }
void setBool(Metadata& out, const std::string& key, bool value) { out.set(key, value); }
void setInt(Metadata& out, const std::string& key, std::int64_t value) { out.set(key, value); }
void setUInt(Metadata& out, const std::string& key, std::uint64_t value) { out.set(key, value); }
void setFloat(Metadata& out, const std::string& key, float value) { out.set(key, value); }
void setFloatArray(Metadata& out, const std::string& key, const std::vector<float>& value) { out.set(key, value); }
void setDoubleArray(Metadata& out, const std::string& key, const std::vector<double>& value) { out.set(key, value); }

std::size_t toIndex1Based(std::int64_t oneBasedIndex, std::size_t size, const std::string& what) {
    require(oneBasedIndex >= 1, what + " must be >= 1");
    const auto idx = static_cast<std::size_t>(oneBasedIndex - 1);
    require(idx < size, what + " out of bounds");
    return idx;
}

bool isLeapYear(std::int64_t year) {
    return ((year % 4) == 0 && (year % 100) != 0) || ((year % 400) == 0);
}

std::int64_t daysInMonth(std::int64_t year, std::int64_t month) {
    static const std::int64_t days[12] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
    require(month >= 1 && month <= 12, "invalid month");
    return month == 2 && isLeapYear(year) ? 29 : days[month - 1];
}

Ymd unpackYyyymmdd(std::uint64_t date) {
    return {static_cast<std::int64_t>(date / 10000ULL),
            static_cast<std::int64_t>((date / 100ULL) % 100ULL),
            static_cast<std::int64_t>(date % 100ULL)};
}

std::uint64_t packYyyymmdd(const Ymd& ymd) {
    return static_cast<std::uint64_t>(ymd.year * 10000 + ymd.month * 100 + ymd.day);
}

Hms secToHms(std::uint64_t sec) {
    return {static_cast<std::int64_t>(sec / 3600ULL),
            static_cast<std::int64_t>((sec / 60ULL) % 60ULL),
            static_cast<std::int64_t>(sec % 60ULL)};
}

std::uint64_t hmsToSec(const Hms& hms) {
    return static_cast<std::uint64_t>(hms.hour * 3600 + hms.minute * 60 + hms.second);
}

std::uint64_t packHhmmss(const Hms& hms) {
    return static_cast<std::uint64_t>(hms.hour * 10000 + hms.minute * 100 + hms.second);
}

Hms unpackHhmmss(std::uint64_t time) {
    return {static_cast<std::int64_t>(time / 10000ULL),
            static_cast<std::int64_t>((time / 100ULL) % 100ULL),
            static_cast<std::int64_t>(time % 100ULL)};
}

void addDays(Ymd& ymd, std::int64_t deltaDays) {
    if (deltaDays >= 0) {
        for (std::int64_t i = 0; i < deltaDays; ++i) {
            ++ymd.day;
            if (ymd.day > daysInMonth(ymd.year, ymd.month)) {
                ymd.day = 1;
                ++ymd.month;
                if (ymd.month > 12) {
                    ymd.month = 1;
                    ++ymd.year;
                }
            }
        }
    }
    else {
        for (std::int64_t i = 0; i < -deltaDays; ++i) {
            --ymd.day;
            if (ymd.day < 1) {
                --ymd.month;
                if (ymd.month < 1) {
                    ymd.month = 12;
                    --ymd.year;
                }
                ymd.day = daysInMonth(ymd.year, ymd.month);
            }
        }
    }
}

std::string toMarsType(const std::string& code) { return code; }
std::string toMarsClass(const std::string& code) { return code; }
std::string toMarsStream(std::int64_t id) { return std::to_string(id); }
std::string toMarsOrigin() { return std::to_string(kOriginEcmwf); }

std::string toMarsPacking(std::int64_t representationId) {
    if (representationId == 1) return "grid_ccsds";
    if (representationId == 2) return "spectral_complex";
    fail("unsupported msg_representation_id");
}

std::string toMarsLevtype(std::int64_t prefixId, std::uint64_t paramId, std::int64_t level, std::int64_t) {
    switch (prefixId) {
        case kPrefixModelLevel:
            if (paramId == 100U || paramId == 101U) return "hl";
            return "ml";
        case kPrefixPressureLevel:
            if (paramId == 100U || paramId == 101U) return "hl";
            return "pl";
        case kPrefixVorticityLevel:
            return "pv";
        case kPrefixThetaLevel:
            return "pt";
        case kPrefixSurface:
            switch (paramId) {
                case 33U:
                case 238U:
                case 228038U:
                case 228141U:
                case 231027U:
                    return level != 0 ? "sol" : "sfc";
                default:
                    return "sfc";
            }
        case kPrefixHeightAboveGround:
            return "hl";
        case kPrefixAl:
            return "al";
        case kPrefixWaveInt:
        case kPrefixWaveSpec:
            return "sfc";
        default:
            fail("unknown prefix id for levtype conversion");
    }
}

std::uint64_t toProcessedDataType(const std::string& simTypeCode) {
    if (simTypeCode == "fc") return 0ULL;
    if (simTypeCode == "an") return 0ULL;
    if (simTypeCode == "cf") return 1ULL;
    if (simTypeCode == "pf") return 2ULL;
    if (simTypeCode == "ssd" || simTypeCode == "gsd") return 3ULL;
    fail("unsupported sim_type_code for processed data type");
}

bool isAnalysis(const Metadata& in) {
    const auto streamId = getInt(in, "sim_stream_id");
    const auto typeCode = getString(in, "sim_type_code");
    if (streamId == 17 || streamId == 18 || streamId == 19 || streamId == 20) return true;
    if ((streamId == 21 || streamId == 22) && (typeCode == "sfo" || typeCode == "fu" || typeCode == "go")) return true;
    return false;
}

bool isEnsemble(const Metadata& in) {
    const auto typeCode = getString(in, "sim_type_code");
    const auto streamId = getInt(in, "sim_stream_id");
    return typeCode == "cf" || typeCode == "pf" || typeCode == "cv" || streamId == 23 || streamId == 19;
}

bool needsPvArray(const Metadata& in) {
    return getInt(in, "msg_prefix_id") == kPrefixModelLevel;
}

bool isSatelliteAtmosphere(const Metadata& in) {
    const auto paramId = getUInt(in, "msg_param_id");
    return paramId == 260508ULL || paramId == 260509ULL || paramId == 260510ULL || paramId == 260511ULL;
}

bool isWaveSpectraWam(const Metadata& in) {
    const auto paramId = getUInt(in, "msg_param_id");
    return paramId == 140250ULL || paramId == 140251ULL;
}

void setTablesVersion(const Metadata& in, Metadata& out) {
    setUInt(out, "misc_tables_version", getUInt(in, "sim_grib2_tables_version_latest"));
}

void setType(const Metadata& in, Metadata& out) { setString(out, "type", toMarsType(getString(in, "sim_type_code"))); }
void setClass(const Metadata& in, Metadata& out) { setString(out, "class", toMarsClass(getString(in, "sim_class_code"))); }
void setExpver(const Metadata& in, Metadata& out) { setString(out, "expver", getString(in, "sim_expver")); }
void setOrigin(const Metadata&, Metadata& out) { setString(out, "origin", toMarsOrigin()); }

void setAnalysis(const Metadata& in, Metadata& out) {
    if (isAnalysis(in)) {
        setUInt(out, "anoffset", getUInt(in, "sim_analysis_offset"));
        setUInt(out, "misc_length_of_time_window", getUInt(in, "sim_analysis_window_length"));
    }
}

void setEnsemble(const Metadata& in, Metadata& out, const std::string& stream) {
    const bool special = stream == "28" || stream == "29" || stream == "30" || stream == "31";
    if (isEnsemble(in)) {
        setUInt(out, "number", getUInt(in, "sim_ensemble_member_number"));
        setUInt(out, "misc_type_of_ensemble_forecast", 1ULL);
        setUInt(out, "misc_number_of_forecasts_in_ensemble", getUInt(in, "sim_ensemble_size"));
    }
    else if (special) {
        setUInt(out, "number", 0ULL);
        setUInt(out, "misc_type_of_ensemble_forecast", 255ULL);
        setUInt(out, "misc_number_of_forecasts_in_ensemble", 0ULL);
    }
}

void setPacking(const Metadata& in, Metadata& out) {
    setString(out, "packing", toMarsPacking(getInt(in, "msg_representation_id")));
}

void setIdentification(const Metadata& in, Metadata& out) {
    setUInt(out, "misc_type_of_processed_data", toProcessedDataType(getString(in, "sim_type_code")));
}

void setSystemMethod(const Metadata& in, Metadata& out, const std::string& stream) {
    const bool writeValues = isEnsemble(in) || stream == "28" || stream == "29" || stream == "30" || stream == "31";
    if (writeValues) {
        setUInt(out, "system", getUInt(in, "sim_system_number"));
        setUInt(out, "method", getUInt(in, "sim_method_number"));
    }
}

void setGeometry(const Metadata& in, Metadata& out) {
    const auto repres = getInt(in, "msg_representation_id");
    if (repres == 1) {
        setString(out, "grid", getString(in, "input_grid_name"));
    }
    else if (repres == 2) {
        setUInt(out, "truncation", getUInt(in, "geo_spectral_truncation"));
        setFloat(out, "misc_laplacian_scale_factor", getFloat(in, "msg_laplacian_scale_factor"));
    }
    else {
        fail("unsupported msg_representation_id");
    }
}

void setAnalysisTime(std::uint64_t date, std::uint64_t time, std::uint64_t step, std::uint64_t timestepInSeconds, Metadata& out) {
    const auto totalSec = hmsToSec(unpackHhmmss(time)) + step * timestepInSeconds;
    const auto deltaDays = totalSec / 86400ULL;
    const auto secOfDay = totalSec % 86400ULL;
    auto ymd = unpackYyyymmdd(date);
    addDays(ymd, static_cast<std::int64_t>(deltaDays));
    setUInt(out, "date", packYyyymmdd(ymd));
    setUInt(out, "time", packHhmmss(secToHms(secOfDay)));
    setUInt(out, "step", 0ULL);
}

TimeProcInfo computeTimeProc(std::uint64_t step, double timestepSeconds, std::uint64_t initialStepHours, bool postprocStepsAreHours,
                             const std::string& typeCode, std::int64_t prevPostprocStep, std::uint64_t paramId) {
    TimeProcInfo out;
    out.secEnd = postprocStepsAreHours ? step * kHoursToSeconds : static_cast<std::uint64_t>(step * timestepSeconds);
    if (typeCode == "fc") out.secEnd += initialStepHours * kHoursToSeconds;
    out.secStart = std::max<std::int64_t>(static_cast<std::int64_t>(prevPostprocStep * timestepSeconds), 0);
    if (paramId == 201ULL || paramId == 202ULL) out.stepType = kTypeOfStatisticalProcessMaximum;
    else if (paramId == 203ULL) out.stepType = kTypeOfStatisticalProcessMinimum;
    else if (paramId == 235ULL || paramId == 228ULL || paramId == 142ULL || paramId == 143ULL) out.stepType = kTypeOfStatisticalProcessAccumulation;
    else if (paramId == 228251ULL || paramId == 228252ULL) out.stepType = kTypeOfStatisticalProcessAverage;
    if (out.stepType == kTypeOfStatisticalProcessInstant) out.secStart = out.secEnd;
    return out;
}

void getBaseDateTime(const Metadata& in, std::uint64_t& date, std::uint64_t& time) {
    auto ymd = unpackYyyymmdd(getUInt(in, "sim_initial_date"));
    auto hms = secToHms(getUInt(in, "sim_initial_time_seconds"));
    hms.minute = 0;
    hms.second = 0;

    const bool condition1 = getString(in, "sim_type_code") == "fc" && getBool(in, "sim_has_obs_config1_term");
    const bool condition2 = getBool(in, "sim_is_variable_resolution_ensemble") && getUInt(in, "sim_vareps_leg_number") >= 2ULL;

    if (condition1) {
        std::int64_t hh = hms.hour - static_cast<std::int64_t>(getUInt(in, "sim_initial_step_hours"));
        if (hh < 0) {
            hh += 24;
            addDays(ymd, -1);
        }
        hms.hour = hh;
    }
    else if (condition2) {
        const auto nfcho = getUInt(in, "sim_truncated_forecast_initial_step_hours");
        std::int64_t days = static_cast<std::int64_t>(nfcho / 24ULL);
        std::int64_t rem = static_cast<std::int64_t>(nfcho % 24ULL);
        std::int64_t hh = hms.hour - rem;
        if (hh < 0) {
            hh += 24;
            ++days;
        }
        hms.hour = hh;
        hms.minute = 0;
        hms.second = 0;
        addDays(ymd, -days);
    }

    date = packYyyymmdd(ymd);
    time = packHhmmss(hms);
}

void setDateTimeAtmosphere(const Metadata& in, Metadata& out) {
    const auto timestep = getUInt(in, "sim_timestep_seconds");
    const auto typeCode = getString(in, "sim_type_code");
    const auto tp = computeTimeProc(getUInt(in, "msg_step"), static_cast<double>(timestep), getUInt(in, "sim_initial_step_hours"),
                                    getBool(in, "sim_postproc_steps_are_hours"), typeCode,
                                    getInt(in, "msg_prev_postproc_step"), getUInt(in, "msg_param_id"));

    setBool(out, "misc_is_valid", tp.valid);
    require(tp.secEnd % kHoursToSeconds == 0ULL, "time proc end time must be multiple of hours");
    require((tp.secEnd - tp.secStart) % kHoursToSeconds == 0ULL, "time proc span must be multiple of hours");
    if (tp.stepType != kTypeOfStatisticalProcessInstant) {
        setUInt(out, "timespan", (tp.secEnd - tp.secStart) / kHoursToSeconds);
    }

    std::uint64_t date = 0;
    std::uint64_t time = 0;
    getBaseDateTime(in, date, time);

    const auto stream = toMarsStream(getInt(in, "sim_stream_id"));
    if (typeCode == "an" || (typeCode == "fc" && stream == "32")) {
        setAnalysisTime(date, time, getUInt(in, "msg_step"), timestep, out);
    }
    else {
        setUInt(out, "date", date);
        setUInt(out, "time", time);
        setUInt(out, "step", tp.secEnd / kHoursToSeconds);
        setUInt(out, "misc_length_of_time_step_in_seconds", timestep);
        setUInt(out, "misc_initial_step", getUInt(in, "sim_initial_step_hours"));
    }
}

void setDateTimeWam(const Metadata& in, Metadata& out) {
    std::uint64_t date = 0;
    std::uint64_t time = 0;
    getBaseDateTime(in, date, time);

    const auto step = getUInt(in, "msg_step");
    const auto timestep = getUInt(in, "sim_timestep_seconds");
    const auto tmp = step * timestep;

    setBool(out, "misc_is_valid", true);
    require(tmp % kHoursToSeconds == 0ULL, "wam step must be multiple of hours");

    if (getString(in, "sim_type_code") == "an") {
        setAnalysisTime(date, time, step, timestep, out);
    }
    else {
        setUInt(out, "date", date);
        setUInt(out, "time", time);
        setUInt(out, "step", tmp / kHoursToSeconds);
        setUInt(out, "misc_length_of_time_step_in_seconds", timestep);
        setUInt(out, "misc_initial_step", getUInt(in, "sim_initial_step_hours"));
    }
}

void populateSharedFieldsAtmosphere(const Metadata& in, Metadata& out) {
    setTablesVersion(in, out);
    setType(in, out);
    setClass(in, out);
    const auto stream = toMarsStream(getInt(in, "sim_stream_id"));
    setString(out, "stream", stream);
    setExpver(in, out);
    setAnalysis(in, out);
    setEnsemble(in, out, stream);
    setString(out, "levtype", toMarsLevtype(getInt(in, "msg_prefix_id"), getUInt(in, "msg_param_id"), getInt(in, "msg_level_index"), getInt(in, "msg_representation_id")));
    setPacking(in, out);
    setOrigin(in, out);
    setIdentification(in, out);
    setSystemMethod(in, out, stream);
    setGeometry(in, out);
}

void populateAtmosphereSpecificFields(const Metadata& in, Metadata& out) {
    if (isSatelliteAtmosphere(in)) {
        const auto idx = toIndex1Based(getInt(in, "msg_level_index"), getIntArray(in, "sat_satellite_ids").size(), "satellite level index");
        const auto satIds = getIntArray(in, "sat_satellite_ids");
        const auto instIds = getIntArray(in, "sat_instrument_ids");
        const auto chanIds = getIntArray(in, "sat_channel_ids");
        const auto seriesIds = getIntArray(in, "sat_series_ids");
        const auto rcwn = getDoubleArray(in, "sat_central_wavenumbers");
        require(idx < instIds.size() && idx < chanIds.size() && idx < seriesIds.size() && idx < rcwn.size(), "satellite arrays size mismatch");
        setUInt(out, "ident", static_cast<std::uint64_t>(satIds[idx]));
        setUInt(out, "instrument", static_cast<std::uint64_t>(instIds[idx]));
        setUInt(out, "channel", static_cast<std::uint64_t>(chanIds[idx]));
        setUInt(out, "misc_satellite_series", static_cast<std::uint64_t>(seriesIds[idx]));
        setUInt(out, "misc_scaled_factor_of_central_vawenumber", 0ULL);
        setUInt(out, "misc_scaled_value_of_central_vawenumber", static_cast<std::uint64_t>(100.0 * std::llround(rcwn[idx])));
    }

    const auto paramId = getUInt(in, "msg_param_id");
    if (paramId >= 400000000ULL && paramId < 500000000ULL) {
        setUInt(out, "param", (paramId / 1000000ULL) * 1000ULL);
        setUInt(out, "chem", (paramId / 1000ULL) % 1000ULL);
    }
    else if (paramId >= 400000ULL && paramId < 500000ULL) {
        setUInt(out, "param", (paramId / 1000ULL) * 1000ULL);
        setUInt(out, "chem", paramId % 1000ULL);
    }
    else {
        setUInt(out, "param", paramId);
    }

    if (paramId == 210001ULL || paramId == 210002ULL || paramId == 210003ULL || paramId == 211001ULL || paramId == 211002ULL ||
        paramId == 211003ULL || paramId == 220001ULL || paramId == 220002ULL || paramId == 220003ULL) {
        setUInt(out, "chem", 3ULL);
    }
    else if (paramId == 260001ULL || paramId == 260002ULL || paramId == 260003ULL || paramId == 260004ULL) {
        setUInt(out, "chem", 0ULL);
    }

    setDateTimeAtmosphere(in, out);

    setInt(out, "levelist", getInt(in, "msg_level_index"));
    if (needsPvArray(in)) {
        const auto zvert = getFloatArray(in, "geo_vertical_coordinates");
        const auto n = static_cast<std::size_t>(2 * (getUInt(in, "geo_num_full_levels") + 1ULL));
        require(zvert.size() >= n, "geo_vertical_coordinates too short for pv array");
        setFloatArray(out, "misc_pv", std::vector<float>(zvert.begin(), zvert.begin() + static_cast<std::ptrdiff_t>(n)));
    }

    setUInt(out, "misc_generating_process_identifier", getUInt(in, "sim_generating_process_identifier"));
}

void populateSharedFieldsWam(const Metadata& in, Metadata& out) {
    setTablesVersion(in, out);
    setType(in, out);
    setClass(in, out);
    const auto stream = toMarsStream(getInt(in, "wam_stream_id"));
    setString(out, "stream", stream);
    setExpver(in, out);
    setAnalysis(in, out);
    setEnsemble(in, out, stream);
    setPacking(in, out);
    setOrigin(in, out);
    setIdentification(in, out);
    setSystemMethod(in, out, stream);
    setGeometry(in, out);
}

void populateWamSpecificFields(const Metadata& in, Metadata& out) {
    setUInt(out, "param", getUInt(in, "msg_param_id"));
    setDateTimeWam(in, out);
    setInt(out, "misc_itmin", getInt(in, "wam_direction_min"));
    setInt(out, "misc_itmax", getInt(in, "wam_direction_max"));

    if (isWaveSpectraWam(in)) {
        setUInt(out, "direction", getUInt(in, "msg_direction_index"));
        setUInt(out, "frequency", getUInt(in, "msg_frequency_index"));
        setString(out, "levtype", "sfc");
        setDoubleArray(out, "misc_wave_dirs", getDoubleArray(in, "wam_directions"));
        setDoubleArray(out, "misc_wave_freqs", getDoubleArray(in, "wam_frequencies"));
    }
    else {
        setString(out, "levtype", "sfc");
        setInt(out, "levelist", 0);
    }

    if (getString(in, "wam_domain_code") == "g") {
        setUInt(out, "misc_generating_process_identifier", getUInt(in, "wam_global_model_id"));
    }
    else {
        setUInt(out, "misc_generating_process_identifier", getUInt(in, "wam_limited_area_model_id"));
    }
}

}  // namespace

bool isWamMetadata(const Metadata& md) {
    if (hasKey(md, "wam_stream_id") || hasKey(md, "wam_domain_code") || hasKey(md, "wam_directions") || hasKey(md, "wam_frequencies")) {
        return true;
    }
    if (hasKey(md, "msg_prefix_id")) {
        const auto prefix = getInt(md, "msg_prefix_id");
        return prefix == kPrefixWaveInt || prefix == kPrefixWaveSpec;
    }
    return false;
}

bool isAtmosphereMetadata(const Metadata& md) {
    if (isWamMetadata(md)) {
        return false;
    }
    return hasKey(md, "sim_stream_id") && hasKey(md, "msg_prefix_id");
}

void atmosphere2mars(const Metadata& in, Metadata& out) {
    populateSharedFieldsAtmosphere(in, out);
    populateAtmosphereSpecificFields(in, out);
}

void wam2mars(const Metadata& in, Metadata& out) {
    populateSharedFieldsWam(in, out);
    populateWamSpecificFields(in, out);
}

}  // namespace multio::action::ifs2mars::detail
