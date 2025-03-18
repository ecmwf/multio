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

#include <cmath>
#include <limits>
#include <map>
#include <unordered_map>
#include <utility>

#include "eckit/config/Configuration.h"
#include "eckit/memory/NonCopyable.h"
#include "eckit/value/Value.h"

#pragma once

namespace multio {

//----------------------------------------------------------------------------------------------------------------------

/// Class todescribe the encoding per field type paramid + levtype
/// Behaves as a POD
struct Encoding {

    int bitsPerValue = 0;
    int decimalScaleFactor = 0;
    float precision = 0;

    /// indentifier for missing decimal scale factor
    static constexpr int marker() { return std::numeric_limits<int>::min(); }

public:
    explicit Encoding(int bpv = 0, int dsf = marker(), float p = 0) :
        bitsPerValue(bpv), decimalScaleFactor(dsf), precision(p) {}

    explicit Encoding(const eckit::Configuration& cfg) {
        bitsPerValue = cfg.getInt("bitsPerValue", 0);
        decimalScaleFactor = cfg.getInt("decimalScaleFactor", marker());
        precision = cfg.getFloat("precision", 0.);
        if (bitsPerValue <= 0 and decimalScaleFactor == marker() and precision <= 0) {
            throw eckit::BadValue("Invalid bitsPerValue or decimalScaleFactor or precision", Here());
        }
    }

    int computeBitsPerValue(double min, double max) const {
        if (bitsPerValue) {
            return bitsPerValue;
        }

        if (decimalScaleFactor != marker()) {
            const int& p = decimalScaleFactor;

            double range = (max - min) * std::pow(10., p);
            int nbits = static_cast<int>(std::ceil(std::log2(range)));
            return nbits;
        }
        else {
            double increments = (max - min) / static_cast<double>(precision);
            int nbits = static_cast<int>(std::ceil(std::log2(increments)));
            return nbits;
        }
    }

    bool defined() const { return (bitsPerValue || decimalScaleFactor != marker() || precision > 0); }

private:
    void print(std::ostream& s) const {
        s << "Encoding(bitsPerValue=" << bitsPerValue << ",decimalScaleFactor=" << decimalScaleFactor
          << ",precision=" << precision << ")";
    }

    friend std::ostream& operator<<(std::ostream& s, const Encoding& v) {
        v.print(s);
        return s;
    }
};

//----------------------------------------------------------------------------------------------------------------------


class EncodingTable;

class EncodeBitsPerValue : private eckit::NonCopyable {
public:
    explicit EncodeBitsPerValue(const eckit::Configuration& config);

    ~EncodeBitsPerValue();

    int getBitsPerValue(int paramid, const std::string& levtype, double min, double max);

private:
    Encoding getEncoding(int paramid, const std::string& levtype);

    void cacheBitsPerValue(int paramid, const std::string& levtype, Encoding e);

    Encoding getCachedBitsPerValue(int paramid, const std::string& levtype);

    Encoding computeBitsPerValue(int paramid, const std::string& levtype);

    Encoding tabulatedBitsPerValue(int paramid, const std::string& levtype);

    /// @todo Remove this hack that was ported from IFS
    int hack(int paramid, const std::string& levtype);

private:
    std::map<std::string, std::unordered_map<int, Encoding>> cache_;

    std::map<std::string, std::unique_ptr<EncodingTable>> tables_;
};

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio
