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


#pragma once

#include <iosfwd>
#include <string>
#include <vector>

#include "fdb5/api/FDB.h"

#include "multio/sink/DataSink.h"

#define HAVE_GRIBJUMP 1 // For now...

#ifdef HAVE_GRIBJUMP
    #include "gribjump/GribJump.h"
    #include "gribjump/info/InfoAggregator.h"
#endif

namespace multio::sink {

//----------------------------------------------------------------------------------------------------------------------

class FDB5Sink : public DataSink {
public:
    explicit FDB5Sink(const config::ComponentConfiguration& compConf);

private:
    void write(eckit::message::Message msg) override;

    void flush() override;

    void print(std::ostream&) const override;

    friend std::ostream& operator<<(std::ostream& s, const FDB5Sink& p) {
        p.print(s);
        return s;
    }

    fdb5::FDB fdb_;

    bool useGribjump_{true};
#ifdef HAVE_GRIBJUMP
    // Maybe this is nicer than using a gribjump object and having to add gribjump.aggregate API.
    gribjump::InfoAggregator cache_; // todo conditionally compile this
#endif
};

}  // namespace multio::sink

//----------------------------------------------------------------------------------------------------------------------
