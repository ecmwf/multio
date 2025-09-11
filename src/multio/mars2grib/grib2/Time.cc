#include "multio/mars2grib/grib2/Time.h"

#include "multio/datamod/core/Record.h"
#include "multio/mars2grib/Mars2GribException.h"


namespace multio::mars2grib::grib2 {

InitForecastTimeKeys setInitForecastTime() {
    InitForecastTimeKeys res;
    dm::applyRecordDefaults(res);
    dm::validateRecord(res);
    return res;
}

DateTimeKeys setDateTime(const dm::FullMarsRecord&) {
    return {};
}
std::optional<RefDateTimeKeys> setRefDateTime(const dm::FullMarsRecord&) {
    return {};
}

std::optional<PointInTimeKeys> setPointInTime(const dm::FullMarsRecord&) {
    return {};
}
std::optional<TimeRangeKeys> setTimeRange(const dm::FullMarsRecord&, const dm::MiscRecord) {
    return {};
}

};  // namespace multio::mars2grib::grib2
