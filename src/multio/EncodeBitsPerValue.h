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

//----------------------------------------------------------------------------------------------------------------------

namespace multio {

class EncodingTable;

class EncodeBitsPerValue : private eckit::NonCopyable {
public:
    EncodeBitsPerValue(const eckit::Configuration& config);
    ~EncodeBitsPerValue();

    int getBitsPerValue(int paramid, const std::string& levtype, double min, double max);

private:
    int getCachedBitsPerValue(int paramid, const std::string& levtype);

    void cacheBitsPerValue(int paramid, const std::string& levtype, int bpv);

    int computeBitsPerValue(int paramid, const std::string& levtype);

    int tabulatedBitsPerValue(int paramid, const std::string& levtype);

    int hack(int paramid, const std::string& levtype);

private:
    std::map<std::string, std::unordered_map<int, int>> cache_;

    std::map<std::string, EncodingTable*> tables_;
};

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio

#endif  // multio_EncodeBitsPerValue_H
