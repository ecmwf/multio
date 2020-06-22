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

#include "eckit/memory/NonCopyable.h"
#include "eckit/config/Configuration.h"

#ifndef multio_EncodeBitsPerValue_H
#define multio_EncodeBitsPerValue_H


//----------------------------------------------------------------------------------------------------------------------

namespace multio {

class EncodeBitsPerValue : private eckit::NonCopyable {
public:

    EncodeBitsPerValue(const eckit::Configuration& config);

    int findBitsPerValue(int paramid, const std::string& levtype);

};

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio

#endif  // multio_EncodeBitsPerValue_H
