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
#include <tuple>
#include <cmath>
#include <algorithm>

#include "EncodeBitsPerValue.h"

#include "eckit/config/LocalConfiguration.h"
#include "eckit/config/YAMLConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/io/DataHandle.h"
#include "eckit/log/Colour.h"
#include "eckit/log/JSON.h"
#include "eckit/log/Log.h"

#include "multio/LibMultio.h"

using namespace eckit;

namespace multio {

//----------------------------------------------------------------------------------------------------------------------

class EncodingTable {
public:
    EncodingTable(const Configuration& cfg) {
        std::cerr << Colour::yellow << "TABLE " << cfg << Colour::reset << std::endl;  ///< FINDME

        for (auto k : cfg.keys()) {
            LocalConfiguration section = cfg.getSubConfiguration(k);
            std::cerr << Colour::yellow << "Section " << section << Colour::reset
                      << std::endl;  ///< FINDME

            Encoding encode(section);

            std::vector<int> paramIDs = section.getIntVector("paramIDs");

            for (auto paramid : paramIDs) {
                bool added;
                std::tie(std::ignore, added) = table_.insert(std::make_pair(paramid, encode));
                if (not added) {
                    std::ostringstream oss;
                    oss << "Encoding entry already exists for paramid " << paramid;
                    throw BadValue(oss.str(), Here());
                }
            }
        }
    }

    Encoding operator()(int paramid) {
        auto e = table_.find(paramid);
        if (e != table_.end())
            return e->second;
        else
            return Encoding{};
    }

private:
    std::map<int, Encoding> table_;

    void print(std::ostream& s) const { s << "EncodingTable(" << table_ << ")"; }

    friend std::ostream& operator<<(std::ostream& s, const EncodingTable& v) {
        v.print(s);
        return s;
    }
};


//----------------------------------------------------------------------------------------------------------------------

EncodeBitsPerValue::EncodeBitsPerValue(const Configuration& config) {
    std::string path;
    char* envtable = ::getenv("MULTIO_ENCODING_TABLE");
    if (envtable) {
        path = envtable;
    }
    else {
        config.get("EncodingBitsPerValueTable", path);
    }

    if (path.empty()) {
        Log::warning()
            << "Path for Encoding BitsPerValue table not configured, MultIO config "
               "key EncodingBitsPerValueTable or env variable MULTIO_ENCODING_TABLE"
            << std::endl;
    }
    else {
        YAMLConfiguration tablecfg{PathName{path}};
        for (auto k : tablecfg.keys()) {
            LocalConfiguration cfg = tablecfg.getSubConfiguration(k);
            tables_[k] = new EncodingTable(cfg);

            std::cerr << Colour::yellow << "Built TABLE --> " << *tables_[k] << Colour::reset
                      << std::endl;  ///< FINDME
        }
    }
}

EncodeBitsPerValue::~EncodeBitsPerValue() {
    for (auto v : tables_) {
        delete v.second;
    }
}

static std::string fix_levtype(const std::string& levtype) {
    std::string result(levtype);

    std::transform(levtype.begin(), levtype.end(), result.begin(),
                   [](unsigned char c) { return std::tolower(c); });

    if (result == "m")
        result = "ml";
    if (result == "p")
        result = "pl";
    if (result == "s" or result == "sf")
        result = "sfc";

    return result;
}

static bool getenv_COMPR_FC_GP_ML() {
    static char* env = ::getenv("COMPR_FC_GP_ML");

    if (env) {

        std::cerr << Colour::bold
                  << "COMPR_FC_GP_ML " << env << " - " << (bool)std::atoi(env)
                  << Colour::reset << std::endl;  ///< FINDME

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

Encoding EncodeBitsPerValue::getCachedBitsPerValue(int paramid, const std::string& lv) {
    auto got = cache_[lv].find(paramid);
    if (got != cache_[lv].end()) {

        std::cerr << Colour::green
                  << "FOUND in CACHE " << got->second
                  << Colour::reset << std::endl;  ///< FINDME

        return got->second;
    }
    return Encoding();
}

void EncodeBitsPerValue::cacheBitsPerValue(int paramid, const std::string& levtype, Encoding e) {
    cache_[levtype][paramid] = e;
}

Encoding EncodeBitsPerValue::tabulatedBitsPerValue(int paramid, const std::string& levtype) {
    auto tableitr = tables_.find(levtype);
    if (tableitr == tables_.end())
        return Encoding();

    EncodingTable& table = *tableitr->second;

    Encoding encode = table(paramid);

    return encode;
}

Encoding EncodeBitsPerValue::computeBitsPerValue(int paramid, const std::string& levtype) {
    int bpv = hack(paramid, levtype);
    return Encoding(bpv);
}

Encoding EncodeBitsPerValue::getEncoding(int paramid, const std::string& lv) {
    Encoding encode;

    // sanitise input
    ASSERT(paramid != 0);
    const std::string levtype = fix_levtype(lv);

    std::cerr << Colour::green << "QUERY levtype " << levtype << " paramid " << paramid
              << Colour::reset << std::endl;  ///< FINDME

    // check cache
    encode = getCachedBitsPerValue(paramid, levtype);
    if (encode.defined()) {
        std::cerr << Colour::green << "FOUND in CACHE " << encode << Colour::reset
                  << std::endl;  ///< FINDME
        return encode;
    }

    // check the tables
    encode = tabulatedBitsPerValue(paramid, levtype);
    if (encode.defined()) {
        std::cerr << Colour::green
                  << "FOUND in TABLE " << encode
                  << Colour::reset << std::endl;  ///< FINDME
        return encode;
    }

    // compute from hacked code or default values
    encode = computeBitsPerValue(paramid, levtype);

    std::cerr << Colour::green
              << "COMPUTED from CODE " << encode
              << Colour::reset << std::endl;  ///< FINDME

    cacheBitsPerValue(paramid, levtype, encode);

    return encode;
}


int EncodeBitsPerValue::getBitsPerValue(int paramid, const std::string& lv, double min,
                                        double max) {
    Encoding encode = getEncoding(paramid, lv);
    return encode.computeBitsPerValue(min, max);
}

}  // namespace multio
