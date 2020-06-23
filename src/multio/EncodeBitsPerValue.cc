/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Tiago Quintino
/// @date June 2020

#include <cstdlib>
#include <fstream>
#include <iosfwd>

#include "multio/DataSink.h"
#include "multio/EncodeBitsPerValue.h"

#include "eckit/exception/Exceptions.h"
#include "eckit/io/DataHandle.h"

using namespace eckit;

//----------------------------------------------------------------------------------------------------------------------

namespace multio {

EncodeBitsPerValue::EncodeBitsPerValue(const Configuration& config) {}

static std::string fix_levtype(const std::string& levtype) {
    std::string result(levtype);

    std::transform(levtype.begin(), levtype.end(), result.begin(),
                   [](unsigned char c) { return std::tolower(c); });

    if (result == "m")
        result = "ml";
    if (result == "p")
        result = "pl";
    if (result == "s")
        result = "sfc";

    return result;
}

static bool getenv_COMPR_FC_GP_ML() {
    static char* env = ::getenv("COMPR_FC_GP_ML");
    if (env) {
        return (bool)std::atoi(env);
    }
    return false;
}

int EncodeBitsPerValue::hack(int paramid, const std::string& levtype) {
    /// @note This code is taken from IFS grib_utils.F90 and represents the old way of setting the
    /// findBitsPerValue
    ///       which we implement here as a last resort, until we move all the coding to be driven by
    ///       the config file

    // CALL GET_ENVIRONMENT_VARIABLE('COMPR_FC_GP_ML', CLCOMPR)
    // IF(KGRIBCD == NGRBCC) THEN
    //   CALL IGRIB_SET_VALUE(KGRIB_HANDLE,'numberOfBitsContainingEachPackedValue',8)
    // ELSEIF(KGRIBCD == NGRBSD.OR.KGRIBCD == NGRBFSR) THEN
    //   CALL IGRIB_SET_VALUE(KGRIB_HANDLE,'numberOfBitsContainingEachPackedValue',24)
    // ELSEIF(KGRIBCD == NGRBCLWC.AND.(CDLTYPE(1:2) == 'PL' .OR. CDLTYPE(1:1) == 'p')) THEN
    //   CALL IGRIB_SET_VALUE(KGRIB_HANDLE,'numberOfBitsContainingEachPackedValue',12)
    // ELSEIF(KGRIBCD == NGRBCIWC.AND.(CDLTYPE(1:2) == 'PL' .OR. CDLTYPE(1:1) == 'p')) THEN
    //   CALL IGRIB_SET_VALUE(KGRIB_HANDLE,'numberOfBitsContainingEachPackedValue',12)
    // ELSEIF(KGRIBCD > 210000 .AND. KGRIBCD < 228000 )  THEN
    //   CALL IGRIB_SET_VALUE(KGRIB_HANDLE,'numberOfBitsContainingEachPackedValue',24)
    // ELSEIF(KGRIBCD == NGRBCLBT .OR. KGRIBCD == NGRBCSBT) THEN
    //   CALL IGRIB_SET_VALUE(KGRIB_HANDLE,'numberOfBitsContainingEachPackedValue',10)
    // ELSEIF (CLCOMPR=="1" .AND. (CDLTYPE(1:2) == 'ML' .OR. CDLTYPE(1:1) == 'm')) THEN
    //   CALL IGRIB_SET_VALUE(KGRIB_HANDLE,'numberOfBitsContainingEachPackedValue',10)
    // ELSE
    //   CALL IGRIB_SET_VALUE(KGRIB_HANDLE,'numberOfBitsContainingEachPackedValue',16)
    // ENDIF
    //
    // These codes are from IFS yom_grib_codes.F90
    const int NGRBCC = 248;       // NGRBCC   - 248 Cloud cover
    const int NGRBSD = 141;       // NGRBSD   - 141 Snow depth
    const int NGRBFSR = 244;      // NGRBFSR  - 244 Forecast surface roughness
    const int NGRBCLWC = 246;     // NGRBCLWC - 246 Cloud liquid water content
    const int NGRBCIWC = 247;     // NGRBCIWC - 247 Cloud ice water content
    const int NGRBCLBT = 260510;  // NGRBCLBT - 260510 Cloudy brightness temperature
    const int NGRBCSBT = 260511;  // NGRBCSBT - 260511 Clear-sky brightness temperature

    if (paramid == NGRBCC)
        return 8;

    if (paramid == NGRBSD or paramid == NGRBFSR)
        return 24;

    if ((paramid == NGRBCLWC or paramid == NGRBCIWC) and levtype == "pl")
        return 12;

    if ((paramid > 210000 and paramid < 228000))
        return 24;

    if (paramid == NGRBCLBT or paramid == NGRBCSBT)
        return 10;

    if (getenv_COMPR_FC_GP_ML() and levtype == "ml")
        return 10;

    return 16;
}

int EncodeBitsPerValue::getCachedBitsPerValue(int paramid, const std::string& lv) {
    auto got = cache_[lv].find(paramid);
    if (got != cache_[lv].end()) {
        return got->second;
    }
    return 0;
}

void EncodeBitsPerValue::cacheBitsPerValue(int paramid, const std::string& levtype, int bpv) {
    cache_[levtype][paramid] = bpv;
}

int EncodeBitsPerValue::computeBitsPerValue(int paramid, const std::string& levtype) {
    return hack(paramid, levtype);
}

int EncodeBitsPerValue::getBitsPerValue(int paramid, const std::string& lv, double min,
                                        double max) {
    int bpv = 0;

    // sanitise input
    ASSERT(paramid != 0);
    const std::string levtype = fix_levtype(lv);

    // check we tables cache
    if ((bpv = getCachedBitsPerValue(paramid, levtype))) {
        return bpv;
    }

    bpv = computeBitsPerValue(paramid, levtype);

    cacheBitsPerValue(paramid, levtype, bpv);

    return bpv;
}

}  // namespace multio
