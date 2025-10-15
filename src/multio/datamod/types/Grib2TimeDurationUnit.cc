/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Grib2TimeDurationUnit.h"
#include "multio/action/encode/GribEncoder.h"
#include "multio/datamod/core/DataModellingException.h"


namespace multio::datamod {

std::string DumpType<Grib2TimeDurationUnit>::dump(Grib2TimeDurationUnit v) {
    switch (v) {
        case Grib2TimeDurationUnit::Minute:
            return "m";
        case Grib2TimeDurationUnit::Hour:
            return "h";
        case Grib2TimeDurationUnit::Day:
            return "D";
        case Grib2TimeDurationUnit::Month:
            return "M";
        case Grib2TimeDurationUnit::Year:
            return "Y";
        case Grib2TimeDurationUnit::Years10:
            return "10Y";
        case Grib2TimeDurationUnit::Years30:
            return "30Y";
        case Grib2TimeDurationUnit::Years100:
            return "C";
        case Grib2TimeDurationUnit::Hours3:
            return "3h";
        case Grib2TimeDurationUnit::Hours6:
            return "6h";
        case Grib2TimeDurationUnit::Hours12:
            return "12h";
        case Grib2TimeDurationUnit::Second:
            return "s";
    }
    throw DataModellingException("DumpType<Grib2TimeDurationUnit>::dump: Unexpected value for Grib2TimeDurationUnit",
                                 Here());
}

std::int64_t DumpType<Grib2TimeDurationUnit, metkit::codes::CodesHandle>::dump(Grib2TimeDurationUnit v) {
    switch (v) {
        case Grib2TimeDurationUnit::Minute:
            return static_cast<std::int64_t>(Grib2TimeDurationUnit::Minute);
        case Grib2TimeDurationUnit::Hour:
            return static_cast<std::int64_t>(Grib2TimeDurationUnit::Hour);
        case Grib2TimeDurationUnit::Day:
            return static_cast<std::int64_t>(Grib2TimeDurationUnit::Day);
        case Grib2TimeDurationUnit::Month:
            return static_cast<std::int64_t>(Grib2TimeDurationUnit::Month);
        case Grib2TimeDurationUnit::Year:
            return static_cast<std::int64_t>(Grib2TimeDurationUnit::Year);
        case Grib2TimeDurationUnit::Years10:
            return static_cast<std::int64_t>(Grib2TimeDurationUnit::Years10);
        case Grib2TimeDurationUnit::Years30:
            return static_cast<std::int64_t>(Grib2TimeDurationUnit::Years30);
        case Grib2TimeDurationUnit::Years100:
            return static_cast<std::int64_t>(Grib2TimeDurationUnit::Years100);
        case Grib2TimeDurationUnit::Hours3:
            return static_cast<std::int64_t>(Grib2TimeDurationUnit::Hours3);
        case Grib2TimeDurationUnit::Hours6:
            return static_cast<std::int64_t>(Grib2TimeDurationUnit::Hours6);
        case Grib2TimeDurationUnit::Hours12:
            return static_cast<std::int64_t>(Grib2TimeDurationUnit::Hours12);
        case Grib2TimeDurationUnit::Second:
            return static_cast<std::int64_t>(Grib2TimeDurationUnit::Second);
    }
    throw DataModellingException("DumpType<Grib2TimeDurationUnit>::dump: Unexpected value for Grib2TimeDurationUnit",
                                 Here());
}

Grib2TimeDurationUnit ParseType<Grib2TimeDurationUnit>::parse(const std::string& val) {
    if (val == "m") {
        return Grib2TimeDurationUnit::Minute;
    }
    if (val == "h") {
        return Grib2TimeDurationUnit::Hour;
    }
    if (val == "D") {
        return Grib2TimeDurationUnit::Day;
    }
    if (val == "M") {
        return Grib2TimeDurationUnit::Month;
    }
    if (val == "Y") {
        return Grib2TimeDurationUnit::Year;
    }
    if (val == "10Y") {
        return Grib2TimeDurationUnit::Years10;
    }
    if (val == "30Y") {
        return Grib2TimeDurationUnit::Years30;
    }
    if (val == "C") {
        return Grib2TimeDurationUnit::Years100;
    }
    if (val == "3h") {
        return Grib2TimeDurationUnit::Hours3;
    }
    if (val == "6h") {
        return Grib2TimeDurationUnit::Hours6;
    }
    if (val == "12h") {
        return Grib2TimeDurationUnit::Hours12;
    }
    if (val == "s") {
        return Grib2TimeDurationUnit::Second;
    }

    throw DataModellingException(
        std::string("ParseType<Grib2TimeDurationUnit>::parse Unknown value for Grib2TimeDurationUnit: ") + val, Here());
}


Grib2TimeDurationUnit ParseType<Grib2TimeDurationUnit>::parse(std::int64_t i) {
    switch (i) {
        case static_cast<std::int64_t>(Grib2TimeDurationUnit::Minute):
            return Grib2TimeDurationUnit::Minute;
        case static_cast<std::int64_t>(Grib2TimeDurationUnit::Hour):
            return Grib2TimeDurationUnit::Hour;
        case static_cast<std::int64_t>(Grib2TimeDurationUnit::Day):
            return Grib2TimeDurationUnit::Day;
        case static_cast<std::int64_t>(Grib2TimeDurationUnit::Month):
            return Grib2TimeDurationUnit::Month;
        case static_cast<std::int64_t>(Grib2TimeDurationUnit::Year):
            return Grib2TimeDurationUnit::Year;
        case static_cast<std::int64_t>(Grib2TimeDurationUnit::Years10):
            return Grib2TimeDurationUnit::Years10;
        case static_cast<std::int64_t>(Grib2TimeDurationUnit::Years30):
            return Grib2TimeDurationUnit::Years30;
        case static_cast<std::int64_t>(Grib2TimeDurationUnit::Years100):
            return Grib2TimeDurationUnit::Years100;
        case static_cast<std::int64_t>(Grib2TimeDurationUnit::Hours3):
            return Grib2TimeDurationUnit::Hours3;
        case static_cast<std::int64_t>(Grib2TimeDurationUnit::Hours6):
            return Grib2TimeDurationUnit::Hours6;
        case static_cast<std::int64_t>(Grib2TimeDurationUnit::Hours12):
            return Grib2TimeDurationUnit::Hours12;
        case static_cast<std::int64_t>(Grib2TimeDurationUnit::Second):
            return Grib2TimeDurationUnit::Second;
        default:
            throw DataModellingException(
                std::string("ParseType<Grib2TimeDurationUnit>::parse Unknown value for Grib2TimeDurationUnit: ")
                    + std::to_string(i),
                Here());
    };
}

}  // namespace multio::datamod


namespace multio {

void util::Print<datamod::Grib2TimeDurationUnit>::print(PrintStream& ps, const datamod::Grib2TimeDurationUnit& t) {
    util::print(ps, datamod::TypeDumper<datamod::Grib2TimeDurationUnit>::dump(t));
}

}  // namespace multio
