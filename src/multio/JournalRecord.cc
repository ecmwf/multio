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

    const FixedString<4> terminationMarker("END!");
    const unsigned char currentTagVersion = 1;

// -------------------------------------------------------------------------------------------------

/// Initialise a (new) Journal record, such that it will be valid for writing
/// (once payload data has been added as appropriate).
void JournalRecord::initialise(RecordType type) {

    eckit::zero(head_);
    head_.tag_ = type;
    head_.tagVersion_ = currentTagVersion;

    SYSCALL(::gettimeofday(&head_.timestamp_, NULL));

    marker_ = terminationMarker;
}


void JournalRecord::writeRecord(DataHandle& handle) {

    handle.write(&head_, sizeof(head_));

    // TODO: We write the payload data here.

    handle.write(&marker_, sizeof(marker_));
}


// -------------------------------------------------------------------------------------------------

} // namespace multio
