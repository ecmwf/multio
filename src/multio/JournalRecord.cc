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

#include <sys/time.h>

#include "eckit/io/DataHandle.h"

#include "JournalRecord.h"

using namespace eckit;

namespace multio {

// -------------------------------------------------------------------------------------------------

void JournalRecord::initialise(RecordType type) {

    head_.tag_ = type;
    head_.tagVersion_ = currentTagVersion;

    SYSCALL(::gettimeofday(&head_.timestamp_, NULL));

}


void JournalRecord::writeRecord(DataHandle& handle) {

}


// -------------------------------------------------------------------------------------------------

} // namespace multio
