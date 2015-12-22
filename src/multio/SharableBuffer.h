/*
 * (C) Copyright 1996-2015 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Simon Smart
/// @date Dec 2015

#ifndef multio_SharableBuffer_H
#define multio_SharableBuffer_H

//
// This defines a Buffer that can be stored in a SharedPtr object.

#include <string>

#include "eckit/io/Buffer.h"
#include "eckit/memory/Owned.h"


namespace multio {

//-------------------------------------------------------------------------------------------------


class SharableBuffer : public eckit::Buffer,
                     public eckit::OwnedLock {

public:
    SharableBuffer(size_t size) : Buffer(size) {}
    SharableBuffer(const std::string& s) : Buffer(s) {}
    SharableBuffer(const char* data, size_t size) : Buffer(data, size) {}

    // And a constructor that doesn't take ownership
    SharableBuffer(void* data, size_t size, bool dummy) : Buffer(data, size, dummy) {}
};


//-------------------------------------------------------------------------------------------------

} // namespace multio

#endif // multio_SharableBuffer_H
