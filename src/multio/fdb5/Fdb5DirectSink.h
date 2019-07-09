/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany
/// @date   June 2019

#ifndef multio_fdb5_Fdb5DirectSink_H
#define multio_fdb5_Fdb5DirectSink_H

#include <iosfwd>
#include <string>
#include <vector>

#include "eckit/memory/NonCopyable.h"
#include "eckit/io/Length.h"
#include "eckit/types/Types.h"

#include "fdb5/api/FDB.h"

#include "multio/DataSink.h"

namespace multio {

class Fdb5DirectSink : public multio::DataSink {

public:

    Fdb5DirectSink(const eckit::Configuration& config);

private:
    void write(eckit::DataBlobPtr blob) override;

    void flush() override;

    void print(std::ostream&) const override;

    friend std::ostream &operator<<(std::ostream &s, const Fdb5DirectSink &p) {
        p.print(s);
        return s;
    }

    fdb5::FDB fdb_;
};

}

#endif
