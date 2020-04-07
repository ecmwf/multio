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
/// @author Domokos Sarmany
/// @date   Dec 2015

#include "multio/fdb5/FDB5Sink.h"

#include "eckit/exception/Exceptions.h"
#include "eckit/types/Metadata.h"

#include "multio/LibMultio.h"

namespace multio {

namespace {
eckit::LocalConfiguration fdb5_configuration(const eckit::Configuration& cfg) {
    auto fdb_config = cfg.getSubConfiguration("config");
    if (not fdb_config.has("useSubToc")) {
        fdb_config.set("useSubToc", true);
    }
    LOG_DEBUG_LIB(LibMultio) << "FDB5 Config = " << fdb_config << std::endl;
    return fdb_config;
}
}  // namespace

FDB5Sink::FDB5Sink(const eckit::Configuration& config) :
    DataSink(config),
    fdb_{fdb5_configuration(config)} {
    LOG_DEBUG_LIB(LibMultio) << "Config = " << config << std::endl;
}

void FDB5Sink::write(eckit::DataBlobPtr blob) {
    LOG_DEBUG_LIB(LibMultio) << "FDB5Sink::write()" << std::endl;

    const eckit::Metadata& md = blob->metadata();

    fdb5::Key key;
    std::string value;
    LOG_DEBUG_LIB(LibMultio) << "metadata: " << md << std::endl;
    for (const auto& kw : md.keywords()) {
        md.get(kw, value);
        key.set(kw, value);
    }

    fdb_.archive(key, blob->buffer(), blob->length());
}

void FDB5Sink::flush() {
    LOG_DEBUG_LIB(LibMultio) << "FDB5Sink::flush()" << std::endl;

    fdb_.flush();
}

void FDB5Sink::print(std::ostream& os) const {
    os << "FDB5Sink()";
}

static DataSinkBuilder<FDB5Sink> FDB5SinkBuilder("fdb5");

}
