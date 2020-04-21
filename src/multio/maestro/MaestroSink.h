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
/// @date   Apr 2020


#ifndef multio_MaestroSink_H
#define multio_MaestroSink_H

#include "multio/sink/DataSink.h"

//--------------------------------------------------------------------------------------------------

namespace multio {

class MaestroSink : public multio::DataSink {
public:
    MaestroSink(const eckit::Configuration& config);
    ~MaestroSink() override;

private:
    void write(eckit::DataBlobPtr blob) override;

    void flush() override;

    void print(std::ostream&) const override;

    friend std::ostream& operator<<(std::ostream& s, const MaestroSink& p) {
        p.print(s);
        return s;
    }

    std::size_t cdoCount_;
};

}  // namespace multio

//--------------------------------------------------------------------------------------------------

#endif
