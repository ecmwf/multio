#include "Ifs2MarsAction.h"

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

#include "eckit/exception/Exceptions.h"
#include "multio/action/Action.h"
#include "multio/message/Metadata.h"
#include "multio/message/Message.h"

namespace multio::action::ifs2mars {
namespace {

using Metadata = multio::message::Metadata;

constexpr std::uint64_t kHoursToSeconds = 3600ULL;
constexpr std::uint64_t kOriginEcmwf = 98ULL;
constexpr std::uint64_t kTypeOfStatisticalProcessInstant = 0ULL;
constexpr std::uint64_t kTypeOfStatisticalProcessAverage = 1ULL;
constexpr std::uint64_t kTypeOfStatisticalProcessAccumulation = 2ULL;
constexpr std::uint64_t kTypeOfStatisticalProcessMaximum = 3ULL;
constexpr std::uint64_t kTypeOfStatisticalProcessMinimum = 4ULL;
constexpr std::uint64_t kTypeOfStatisticalProcessMode = 5ULL;
constexpr std::uint64_t kTypeOfStatisticalProcessSeverity = 6ULL;

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

std::size_t toIndex1Based(std::int64_t oneBasedIndex, std::size_t size, const std::string& what) {
    require(oneBasedIndex >= 1, what + " must be >= 1");
    const std::size_t idx = static_cast<std::size_t>(oneBasedIndex - 1);
    require(idx < size, what + " out of bounds");
    return idx;
}

template <typename T>
T getValue(const Metadata& in, const std::string& key) {
    return in.get<T>(key);
}

std::string getString(const Metadata& in, const std::string& key) {
    return getValue<std::string>(in, key);
}

bool getBool(const Metadata& in, const std::string& key) {
    return getValue<bool>(in, key);
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
    fail("key '" + key + "' is not an integer");
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
    fail("key '" + key + "' is not an unsigned integer");
}

float getFloat(const Metadata& in, const std::string& key) {
    if (auto v = in.getOpt<float>(key)) {
        return *v;
    }
    if (auto v = in.getOpt<double>(key)) {
        return static_cast<float>(*v);
    }
    fail("key '" + key + "' is not a float");
}

double getDouble(const Metadata& in, const std::string& key) {
    if (auto v = in.getOpt<double>(key)) {
        return *v;
    }
    if (auto v = in.getOpt<float>(key)) {
        return static_cast<double>(*v);
    }
    fail("key '" + key + "' is not a double");
}

std::vector<std::int64_t> getIntArray(const Metadata& in, const std::string& key) {
    if (auto v = in.getOpt<std::vector<std::int64_t>>(key)) {
        return *v;
    }
    if (auto v = in.getOpt<std::vector<long>>(key)) {
        std::vector<std::int64_t> out;
        out.reserve(v->size());
        for (const auto& x : *v) {
            out.push_back(static_cast<std::int64_t>(x));
        }
        return out;
    }
    if (auto v = in.getOpt<std::vector<int>>(key)) {
        std::vector<std::int64_t> out;
        out.reserve(v->size());
        for (const auto& x : *v) {
            out.push_back(static_cast<std::int64_t>(x));
        }
        return out;
    }
    fail("key '" + key + "' is not an integer array");
}

std::vector<float> getFloatArray(const Metadata& in, const std::string& key) {
    if (auto v = in.getOpt<std::vector<float>>(key)) {
        return *v;
    }
    if (auto v = in.getOpt<std::vector<double>>(key)) {
        std::vector<float> out;
        out.reserve(v->size());
        for (const auto& x : *v) {
            out.push_back(static_cast<float>(x));
        }
        return out;
    }
    fail("key '" + key + "' is not a float array");
}

std::vector<double> getDoubleArray(const Metadata& in, const std::string& key) {
    if (auto v = in.getOpt<std::vector<double>>(key)) {
        return *v;
    }
    if (auto v = in.getOpt<std::vector<float>>(key)) {
        std::vector<double> out;
        out.reserve(v->size());
        for (const auto& x : *v) {
            out.push_back(static_cast<double>(x));
        }
        return out;
    }
    fail("key '" + key + "' is not a double array");
}

void setString(Metadata& out, const std::string& key, const std::string& value) {
    out.set(key, value);
}

void setBool(Metadata& out, const std::string& key, bool value) {
    out.set(key, value);
}

void setInt(Metadata& out, const std::string& key, std::int64_t value) {
    out.set(key, value);
}

void setUInt(Metadata& out, const std::string& key, std::uint64_t value) {
    out.set(key, value);
}

void setFloat(Metadata& out, const std::string& key, float value) {
    out.set(key, value);
}

void setDoubleArray(Metadata& out, const std::string& key, const std::vector<double>& value) {
    out.set(key, value);
}

void setFloatArray(Metadata& out, const std::string& key, const std::vector<float>& value) {
    out.set(key, value);
}

bool isLeapYear(std::int64_t year) {
    return ((year % 4) == 0 && (year % 100) != 0) || ((year % 400) == 0);
}

std::int64_t daysInMonth(std::int64_t year, std::int64_t month) {
    static const std::int64_t days[12] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
    require(month >= 1 && month <= 12, "invalid month");
    if (month == 2 && isLeapYear(year)) {
        return 29;
    }
    return days[month - 1];
}

Ymd unpackYyyymmdd(std::uint64_t date) {
    return Ymd{static_cast<std::int64_t>(date / 10000ULL),
               static_cast<std::int64_t>((date / 100ULL) % 100ULL),
               static_cast<std::int64_t>(date % 100ULL)};
}

std::uint64_t packYyyymmdd(const Ymd& ymd) {
    return static_cast<std::uint64_t>(ymd.year * 10000 + ymd.month * 100 + ymd.day);
}

Hms secToHms(std::uint64_t sec) {
    Hms out;
    out.hour = static_cast<std::int64_t>(sec / 3600ULL);
    sec %= 3600ULL;
    out.minute = static_cast<std::int64_t>(sec / 60ULL);
    out.second = static_cast<std::int64_t>(sec % 60ULL);
    return out;
}

std::uint64_t hmsToSec(const Hms& hms) {
    return static_cast<std::uint64_t>(hms.hour * 3600 + hms.minute * 60 + hms.second);
}

std::uint64_t packHhmmss(const Hms& hms) {
    return static_cast<std::uint64_t>(hms.hour * 10000 + hms.minute * 100 + hms.second);
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

std::string toMarsType(const std::string& simTypeCode) { return simTypeCode; }
std::string toMarsClass(const std::string& simClassCode) { return simClassCode; }
std::string toMarsStream(std::int64_t streamId) { return std::to_string(streamId); }
std::string toMarsOrigin() { return std::to_string(kOriginEcmwf); }

std::string toMarsLevtype(std::int64_t prefixId, std::uint64_t, std::int64_t, std::int64_t) {
    return std::to_string(prefixId);
}

std::string toMarsPacking(std::int64_t representationId) {
    if (representationId == 1) {
        return "grid_ccsds";
    }
    if (representationId == 2) {
        return "spectral_complex";
    }
    fail("unknown representation id for packing");
}

std::uint64_t toProcessedDataType(const std::string& simTypeCode) {
    if (simTypeCode == "fc" || simTypeCode == "an") {
        return 0ULL;
    }
    if (simTypeCode == "cf") {
        return 1ULL;
    }
    if (simTypeCode == "pf") {
        return 2ULL;
    }
    if (simTypeCode == "ssd" || simTypeCode == "gsd") {
        return 3ULL;
    }
    fail("unsupported sim_type_code for processed data type");
}

bool isAnalysis(const Metadata& in) {
    const auto streamId = getInt(in, "sim_stream_id");
    const auto typeCode = getString(in, "sim_type_code");

    if (streamId == 17 || streamId == 18 || streamId == 19 || streamId == 20) {
        return true;
    }

    if ((streamId == 21 || streamId == 22) && (typeCode == "sfo" || typeCode == "fu" || typeCode == "go")) {
        return true;
    }

    return false;
}

bool isEnsemble(const Metadata& in) {
    const auto typeCode = getString(in, "sim_type_code");
    const auto streamId = getInt(in, "sim_stream_id");
    return typeCode == "cf" || typeCode == "pf" || typeCode == "cv" || streamId == 23 || streamId == 19;
}

bool needsPvArray(const Metadata& in) {
    return getInt(in, "msg_prefix_id") == 1;
}

void setTablesVersion(const Metadata& in, Metadata& out) {
    setUInt(out, "misc_tables_version", getUInt(in, "sim_grib2_tables_version_latest"));
}

void setType(const Metadata& in, Metadata& out) {
    setString(out, "type", toMarsType(getString(in, "sim_type_code")));
}

void setClass(const Metadata& in, Metadata& out) {
    setString(out, "class", toMarsClass(getString(in, "sim_class_code")));
}

void setExpver(const Metadata& in, Metadata& out) {
    setString(out, "expver", getString(in, "sim_expver"));
}

void setAnalysis(const Metadata& in, Metadata& out) {
    if (isAnalysis(in)) {
        setUInt(out, "anoffset", getUInt(in, "sim_analysis_offset"));
        setUInt(out, "misc_length_of_time_window", getUInt(in, "sim_analysis_window_length"));
    }
}

void setEnsemble(const Metadata& in, Metadata& out) {
    const auto stream = getString(out, "stream");
    const bool special = (stream == "28" || stream == "29" || stream == "30" || stream == "31");

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

void setOrigin(const Metadata&, Metadata& out) {
    setString(out, "origin", toMarsOrigin());
}

void setIdentification(const Metadata& in, Metadata& out) {
    setUInt(out, "misc_type_of_processed_data", toProcessedDataType(getString(in, "sim_type_code")));
}

void setSystemMethod(const Metadata& in, Metadata& out) {
    const auto stream = getString(out, "stream");
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

bool isSatelliteAtmosphere(const Metadata& in) {
    const auto paramId = getUInt(in, "msg_param_id");
    return paramId == 260508ULL || paramId == 260509ULL || paramId == 260510ULL || paramId == 260511ULL;
}

void setStreamAtmosphere(const Metadata& in, Metadata& out) {
    setString(out, "stream", toMarsStream(getInt(in, "sim_stream_id")));
}

void setLevtypeAtmosphere(const Metadata& in, Metadata& out) {
    setString(out,
              "levtype",
              toMarsLevtype(getInt(in, "msg_prefix_id"),
                            getUInt(in, "msg_param_id"),
                            getInt(in, "msg_level_index"),
                            getInt(in, "msg_representation_id")));
}

void setSatelliteAtmosphere(const Metadata& in, Metadata& out) {
    if (!isSatelliteAtmosphere(in)) {
        return;
    }

    const auto satIds = getIntArray(in, "sat_satellite_ids");
    const auto instIds = getIntArray(in, "sat_instrument_ids");
    const auto chanIds = getIntArray(in, "sat_channel_ids");
    const auto seriesIds = getIntArray(in, "sat_series_ids");
    const auto rcwn = getDoubleArray(in, "sat_central_wavenumbers");

    const auto idx = toIndex1Based(getInt(in, "msg_level_index"), satIds.size(), "satellite level index");
    require(idx < instIds.size() && idx < chanIds.size() && idx < seriesIds.size() && idx < rcwn.size(),
            "satellite arrays size mismatch");

    setUInt(out, "ident", static_cast<std::uint64_t>(satIds[idx]));
    setUInt(out, "instrument", static_cast<std::uint64_t>(instIds[idx]));
    setUInt(out, "channel", static_cast<std::uint64_t>(chanIds[idx]));
    setUInt(out, "misc_satellite_series", static_cast<std::uint64_t>(seriesIds[idx]));
    setUInt(out, "misc_scaled_factor_of_central_vawenumber", 0ULL);
    setUInt(out, "misc_scaled_value_of_central_vawenumber", static_cast<std::uint64_t>(100.0 * std::llround(rcwn[idx])));
}

void setModelAtmosphere(const Metadata& in, Metadata& out) {
    setUInt(out, "misc_generating_process_identifier", getUInt(in, "sim_generating_process_identifier"));
}

void setParamAtmosphere(const Metadata& in, Metadata& out) {
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

    if (paramId == 210001ULL || paramId == 210002ULL || paramId == 210003ULL || paramId == 211001ULL ||
        paramId == 211002ULL || paramId == 211003ULL || paramId == 220001ULL || paramId == 220002ULL ||
        paramId == 220003ULL) {
        setUInt(out, "chem", 3ULL);
    }
    else if (paramId == 260001ULL || paramId == 260002ULL || paramId == 260003ULL || paramId == 260004ULL) {
        setUInt(out, "chem", 0ULL);
    }
}

void setAnalysisTime(std::uint64_t date,
                     std::uint64_t time,
                     std::uint64_t step,
                     std::uint64_t timestepInSeconds,
                     Metadata& out) {
    auto hms = Hms{static_cast<std::int64_t>(time / 10000ULL),
                   static_cast<std::int64_t>((time / 100ULL) % 100ULL),
                   static_cast<std::int64_t>(time % 100ULL)};
    std::uint64_t totalSec = hmsToSec(hms) + step * timestepInSeconds;
    const auto deltaDays = totalSec / 86400ULL;
    const auto secOfDay = totalSec % 86400ULL;

    auto ymd = unpackYyyymmdd(date);
    addDays(ymd, static_cast<std::int64_t>(deltaDays));

    setUInt(out, "date", packYyyymmdd(ymd));
    setUInt(out, "time", packHhmmss(secToHms(secOfDay)));
    setUInt(out, "step", 0ULL);
}

TimeProcInfo computeTimeProc(std::uint64_t step,
                             double timestepSeconds,
                             std::uint64_t initialStepHours,
                             bool postprocStepsAreHours,
                             const std::string& typeCode,
                             std::int64_t prevPostprocStep,
                             std::uint64_t paramId) {
    TimeProcInfo out;

    if (postprocStepsAreHours) {
        out.secEnd = step * kHoursToSeconds;
    }
    else {
        out.secEnd = static_cast<std::uint64_t>(step * timestepSeconds);
    }

    if (typeCode == "fc") {
        out.secEnd += initialStepHours * kHoursToSeconds;
    }

    out.secStart = std::max<std::int64_t>(static_cast<std::int64_t>(prevPostprocStep * timestepSeconds), 0);
    out.stepType = kTypeOfStatisticalProcessInstant;

    if (paramId == 201ULL || paramId == 202ULL) {
        out.stepType = kTypeOfStatisticalProcessMaximum;
    }
    else if (paramId == 203ULL) {
        out.stepType = kTypeOfStatisticalProcessMinimum;
    }
    else if (paramId == 235ULL || paramId == 228ULL || paramId == 142ULL || paramId == 143ULL) {
        out.stepType = kTypeOfStatisticalProcessAccumulation;
    }
    else if (paramId == 228251ULL || paramId == 228252ULL) {
        out.stepType = kTypeOfStatisticalProcessAverage;
    }

    if (out.stepType == kTypeOfStatisticalProcessInstant) {
        out.secStart = out.secEnd;
    }

    return out;
}

void setDateTimeAtmosphere(const Metadata& in, Metadata& out) {
    auto ymd = unpackYyyymmdd(getUInt(in, "sim_initial_date"));
    auto hms = secToHms(getUInt(in, "sim_initial_time_seconds"));
    hms.minute = 0;
    hms.second = 0;

    const bool condition1 = getString(in, "sim_type_code") == "fc" && getBool(in, "sim_has_obs_config1_term");
    const bool condition2 = getBool(in, "sim_is_variable_resolution_ensemble") &&
                            getUInt(in, "sim_vareps_leg_number") >= 2ULL;

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

    const auto date = packYyyymmdd(ymd);
    const auto time = packHhmmss(hms);
    const auto timestep = getUInt(in, "sim_timestep_seconds");

    const auto tp = computeTimeProc(getUInt(in, "msg_step"),
                                    static_cast<double>(timestep),
                                    getUInt(in, "sim_initial_step_hours"),
                                    getBool(in, "sim_postproc_steps_are_hours"),
                                    getString(in, "sim_type_code"),
                                    getInt(in, "msg_prev_postproc_step"),
                                    getUInt(in, "msg_param_id"));

    setBool(out, "misc_is_valid", tp.valid);
    require(tp.secEnd % kHoursToSeconds == 0ULL, "time proc end time must be multiple of hours");
    require((tp.secEnd - tp.secStart) % kHoursToSeconds == 0ULL, "time proc span must be multiple of hours");

    if (tp.stepType != kTypeOfStatisticalProcessInstant) {
        setUInt(out, "timespan", (tp.secEnd - tp.secStart) / kHoursToSeconds);
    }

    if (getString(in, "sim_type_code") == "an" ||
        (getString(in, "sim_type_code") == "fc" && getString(out, "stream") == "32")) {
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

void setLevelistAtmosphere(const Metadata& in, Metadata& out) {
    setInt(out, "levelist", getInt(in, "msg_level_index"));
    if (needsPvArray(in)) {
        const auto zvert = getFloatArray(in, "geo_vertical_coordinates");
        const auto n = static_cast<std::size_t>(2 * (getUInt(in, "geo_num_full_levels") + 1ULL));
        require(zvert.size() >= n, "geo_vertical_coordinates too short for pv array");
        std::vector<float> pv(zvert.begin(), zvert.begin() + static_cast<std::ptrdiff_t>(n));
        setFloatArray(out, "misc_pv", pv);
    }
}

bool isWaveSpectraWam(const Metadata& in) {
    const auto paramId = getUInt(in, "msg_param_id");
    return paramId == 140250ULL || paramId == 140251ULL;
}

void setStreamWam(const Metadata& in, Metadata& out) {
    setString(out, "stream", toMarsStream(getInt(in, "wam_stream_id")));
}

void setModelWam(const Metadata& in, Metadata& out) {
    if (getString(in, "wam_domain_code") == "g") {
        setUInt(out, "misc_generating_process_identifier", getUInt(in, "wam_global_model_id"));
    }
    else {
        setUInt(out, "misc_generating_process_identifier", getUInt(in, "wam_limited_area_model_id"));
    }
}

void setParamWam(const Metadata& in, Metadata& out) {
    setUInt(out, "param", getUInt(in, "msg_param_id"));
}

void setDateTimeWam(const Metadata& in, Metadata& out) {
    auto ymd = unpackYyyymmdd(getUInt(in, "sim_initial_date"));
    auto hms = secToHms(getUInt(in, "sim_initial_time_seconds"));
    hms.minute = 0;
    hms.second = 0;

    const bool condition1 = getString(in, "sim_type_code") == "fc" && getBool(in, "sim_has_obs_config1_term");
    const bool condition2 = getBool(in, "sim_is_variable_resolution_ensemble") &&
                            getUInt(in, "sim_vareps_leg_number") >= 2ULL;

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

    const auto date = packYyyymmdd(ymd);
    const auto time = packHhmmss(hms);
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

void setDirFreqWam(const Metadata& in, Metadata& out) {
    setInt(out, "misc_itmin", getInt(in, "wam_direction_min"));
    setInt(out, "misc_itmax", getInt(in, "wam_direction_max"));

    setString(out, "levtype", "sfc");

    if (isWaveSpectraWam(in)) {
        setUInt(out, "direction", getUInt(in, "msg_direction_index"));
        setUInt(out, "frequency", getUInt(in, "msg_frequency_index"));
        setDoubleArray(out, "misc_wave_dirs", getDoubleArray(in, "wam_directions"));
        setDoubleArray(out, "misc_wave_freqs", getDoubleArray(in, "wam_frequencies"));
    }
    else {
        setInt(out, "levelist", 0);
    }
}

void populateSharedFieldsAtmosphere(const Metadata& in, Metadata& out) {
    setTablesVersion(in, out);
    setType(in, out);
    setClass(in, out);
    setStreamAtmosphere(in, out);
    setExpver(in, out);
    setAnalysis(in, out);
    setEnsemble(in, out);
    setLevtypeAtmosphere(in, out);
    setPacking(in, out);
    setOrigin(in, out);
    setIdentification(in, out);
    setSystemMethod(in, out);
    setGeometry(in, out);
}

void populateSharedFieldsWam(const Metadata& in, Metadata& out) {
    setTablesVersion(in, out);
    setType(in, out);
    setClass(in, out);
    setStreamWam(in, out);
    setExpver(in, out);
    setAnalysis(in, out);
    setEnsemble(in, out);
    setPacking(in, out);
    setOrigin(in, out);
    setIdentification(in, out);
    setSystemMethod(in, out);
    setGeometry(in, out);
}

void populateAtmosphereSpecificFields(const Metadata& in, Metadata& out) {
    setSatelliteAtmosphere(in, out);
    setParamAtmosphere(in, out);
    setDateTimeAtmosphere(in, out);
    setLevelistAtmosphere(in, out);
    setModelAtmosphere(in, out);
}

void populateWamSpecificFields(const Metadata& in, Metadata& out) {
    setParamWam(in, out);
    setDateTimeWam(in, out);
    setDirFreqWam(in, out);
    setModelWam(in, out);
}

bool looksLikeWam(const Metadata& md) {
    return md.getOpt<std::int64_t>("wam_stream_id").has_value() ||
           md.getOpt<std::uint64_t>("wam_stream_id").has_value() ||
           md.getOpt<std::string>("wam_domain_code").has_value();
}

void updateMarsMetadataAtmosphere(const Metadata& in, Metadata& out) {
    populateSharedFieldsAtmosphere(in, out);
    populateAtmosphereSpecificFields(in, out);
}

void updateMarsMetadataWam(const Metadata& in, Metadata& out) {
    populateSharedFieldsWam(in, out);
    populateWamSpecificFields(in, out);
}

}  // namespace

Ifs2Mars::Ifs2Mars(const ComponentConfiguration& compConf) :
    ChainedAction(compConf),
    mode_{cf::parseActionConfig<Ifs2MarsConfig>(compConf).mode.value_or(Ifs2MarsMode::Auto)} {}

void Ifs2Mars::executeImpl(message::Message msg) {
    if (msg.tag() != message::Message::Tag::Field) {
        executeNext(std::move(msg));
        return;
    }

    msg.acquireMetadata();
    const auto in = msg.metadata();
    auto& md = msg.modifyMetadata();

    switch (mode_) {
        case Ifs2MarsMode::Atmosphere:
            updateMarsMetadataAtmosphere(in, md);
            break;
        case Ifs2MarsMode::Wam:
            updateMarsMetadataWam(in, md);
            break;
        case Ifs2MarsMode::Auto:
            if (looksLikeWam(in)) {
                updateMarsMetadataWam(in, md);
            }
            else {
                updateMarsMetadataAtmosphere(in, md);
            }
            break;
    }

    executeNext(std::move(msg));
}

void Ifs2Mars::print(std::ostream& os) const {
    os << "Ifs2Mars Action";
}

static ActionBuilder<Ifs2Mars> Ifs2MarsBuilder("ifs2mars");

}  // namespace multio::action::ifs2mars

template <>
struct multio::util::config::detail::EnumTrait<multio::action::ifs2mars::Ifs2MarsMode> {
    static constexpr std::array values{
        std::pair{multio::action::ifs2mars::Ifs2MarsMode::Auto, "auto"},
        std::pair{multio::action::ifs2mars::Ifs2MarsMode::Atmosphere, "atmosphere"},
        std::pair{multio::action::ifs2mars::Ifs2MarsMode::Wam, "wam"},
    };
};
