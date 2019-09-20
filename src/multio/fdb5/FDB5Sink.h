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
/// @date   Dec 2015


#ifndef multio_fdb5_FDB5Sink_H
#define multio_fdb5_FDB5Sink_H

#include <iosfwd>
#include <string>
#include <vector>

#include "eckit/memory/NonCopyable.h"
#include "eckit/io/Length.h"
#include "eckit/types/Types.h"

#include "multio/DataSink.h"

#include "fdb5/legacy/LegacyArchiver.h"

//--------------------------------------------------------------------------------------------------

class FDB5Sink : public multio::DataSink {

public:

    FDB5Sink(const eckit::Configuration& config);

    virtual ~FDB5Sink();

    virtual void write(eckit::DataBlobPtr blob);
    virtual void flush();

protected:

    virtual void print(std::ostream&) const;

private:

    friend std::ostream &operator<<(std::ostream &s, const FDB5Sink &p) {
        p.print(s);
        return s;
    }

private: // members
    fdb5::legacy::LegacyArchiver archiver_;
};

//--------------------------------------------------------------------------------------------------

#endif
