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

#include <map>
#include <unordered_map>
#include <utility>

#include "eckit/config/Configuration.h"
#include "eckit/memory/NonCopyable.h"
#include "eckit/value/Value.h"

#ifndef multio_EncodeBitsPerValue_H
#define multio_EncodeBitsPerValue_H

namespace multio {

//----------------------------------------------------------------------------------------------------------------------

/// Class todescribe the encoding per field type paramid + levtype
/// This behaves as a POD
///
class Encoding {
public:
    explicit Encoding(int bpv = 0, int dsf = 0) : bitsPerValue_(bpv), decimalScaleFactor_(dsf) {}

    explicit Encoding(const eckit::Configuration& cfg) {
        bitsPerValue_ = cfg.getInt("bitsPerValue", 0);
        decimalScaleFactor_ = cfg.getInt("decimalScaleFactor", 0);
        if (bitsPerValue_ <= 0 and decimalScaleFactor_ <= 0) {
            throw eckit::BadValue("Invalid bitsPerValue or decimalScaleFactor", Here());
        }
    }

    int computeBitsPerValue(double min, double max) const {
        if (bitsPerValue_)
            return bitsPerValue_;

        const int& p = decimalScaleFactor_;

        double range = (max - min) * std::pow(10., p);
        int nbits = static_cast<int>(std::ceil(std::log2(range)));
        return nbits;
    }

    // All zeros mean this Encoding is undefined
    bool defined() const { return (bitsPerValue_ || decimalScaleFactor_);  }

private:

    void print(std::ostream& s) const {
        s << "Encoding(bitsPerValue=" << bitsPerValue_
          << ",decimalScaleFactor=" << decimalScaleFactor_ << ")";
    }

    friend std::ostream& operator<<(std::ostream& s, const Encoding& v) {
        v.print(s);
        return s;
    }

private:

    int bitsPerValue_ = 0;
    int decimalScaleFactor_ = 0;

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

    std::map<std::string, EncodingTable*> tables_;
};

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio

#endif  // multio_EncodeBitsPerValue_H
