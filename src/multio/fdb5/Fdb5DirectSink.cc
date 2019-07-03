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

#include "multio/fdb5/Fdb5DirectSink.h"

#include "eckit/exception/Exceptions.h"
#include "eckit/types/Metadata.h"

#include "multio/LibMultio.h"

namespace multio {

namespace {
eckit::LocalConfiguration fdb5_configuration(const eckit::Configuration& cfg) {
    return cfg.has("config") ? cfg.getSubConfiguration("config") : eckit::LocalConfiguration{};
}
}  // namespace

Fdb5DirectSink::Fdb5DirectSink(const eckit::Configuration& config) :
    DataSink(config),
    fdb_{fdb5_configuration(config)} {}

void Fdb5DirectSink::write(eckit::DataBlobPtr blob) {
    const eckit::Metadata& md = blob->metadata();

    fdb5::Key key;
    std::string value;
    for (const auto& kw : md.keywords()) {
        md.get(kw, value);
        key.set(kw, value);
    }

    fdb_.archive(key, blob->buffer(), blob->length());
}

void Fdb5DirectSink::flush() {
    fdb_.flush();
}

void Fdb5DirectSink::print(std::ostream& os) const {
    os << "Fdb5DirectSink()";
}

static DataSinkBuilder<Fdb5DirectSink> Fdb5DirectSinkBuilder("fdb5-direct");

}
