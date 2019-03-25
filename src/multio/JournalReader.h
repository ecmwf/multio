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
/// @author Simon Smart
/// @date Dec 2015


#ifndef multio_JournalReader_H
#define multio_JournalReader_H

#include <iosfwd>

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/io/DataHandle.h"

#include "multio/Journal.h"
#include "multio/JournalRecord.h"

namespace multio {

// -------------------------------------------------------------------------------------------------

class JournalReader : public Journal {

public: // methods

    JournalReader(const eckit::Configuration& config, const eckit::PathName& path);

    ~JournalReader();

    bool readRecord(JournalRecord& record);

    int readEvents() const;

    int readWriteRecords() const;

    const eckit::Configuration& config() const;

protected: // methods

    void print(std::ostream&) const;

private: // methods

    friend std::ostream &operator<<(std::ostream &s, const JournalReader &p) {
        p.print(s);
        return s;
    }

    void readConfiguration();

private:

    eckit::PathName path_;
    std::unique_ptr<eckit::DataHandle> handle_;

    int nReadWriteRecords_; // This excludes the header and footer.
    int nReadEvents_;

    std::unique_ptr<eckit::YAMLConfiguration> config_;
};

// -------------------------------------------------------------------------------------------------

} // namespace multio

#endif // multio_JournalReader_H
