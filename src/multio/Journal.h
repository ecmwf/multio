/*
 * (C) Copyright 1996-2015 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Tiago Quintino
/// @author Simon Smart
/// @date Dec 2015


#ifndef multio_Journal_H
#define multio_Journal_H

#include <iosfwd>
#include <string>
#include <vector>
#include <sys/time.h>

#include "eckit/config/Configuration.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/io/DataHandle.h"
#include "eckit/io/Length.h"
#include "eckit/memory/NonCopyable.h"
#include "eckit/memory/ScopedPtr.h"
#include "eckit/thread/Mutex.h"
#include "eckit/types/FixedString.h"

#include "multio/JournalRecord.h"

namespace multio {

//-------------------------------------------------------------------------------------------------

class Journal : private eckit::NonCopyable {

public: // constants

    const static eckit::FixedString<8> CurrentHeaderTag;
    const static unsigned char CurrentVersion;

public: // types

    struct Header {

        eckit::FixedString<8> tag_;             // (8)   Magic tag (IOJOU999) [I/O Journal 999]
        unsigned char         tagVersion_;      // (1)   Identify journal version.
        unsigned char         unused_[3];       // (3)   Reserved for future use. 

        timeval               timestamp_;       // (16) Time of creation of journal (in unix seconds)

        unsigned char         unused2_[116];    // (116) reserved for future use.
    } head_;

    JournalRecord footer_;

public: // methods

    Journal(const eckit::Configuration& config);

    ~Journal();

    /// If the journal is not yet open, then open it and write header info.
    void open();

    /// If the journal is open, finalise and close it.
    void close();

    // n.b. we separate the call for journaling data, which is always done in the same
    //      way, from journaling the journal entries, which are specific to each of
    //      the data sinks.

    void write_record(JournalRecord& record);

protected: // methods

    void print(std::ostream&) const;

private: // methods

    void init_header();

    friend std::ostream &operator<<(std::ostream &s, const Journal &p) {
        p.print(s);
        return s;
    }

private: // members

    eckit::PathName path_;
    eckit::ScopedPtr<eckit::DataHandle> handle_;
    eckit::Mutex mutex_;

    bool isOpen_;
};

//-------------------------------------------------------------------------------------------------

}  // namespace multio

#endif

