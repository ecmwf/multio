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
#include "multio/LibMultio.h"

#include "fdb5/config/Config.h"
#include "eckit/config/LocalConfiguration.h"
#include "multio/util/Substitution.h"
#include "eckit/value/Value.h"


namespace multio::sink {

using config::ComponentConfiguration;

namespace {

void replaceCurly( const ComponentConfiguration& compConf, eckit::LocalConfiguration& cfg ){
    for (auto& key : cfg.keys()) {
        // Replace the value if it is string
        if ( cfg.getSubConfiguration(key).get().isString() ){
            cfg.set(key, compConf.multioConfig().replaceCurly(cfg.getString(key)) );
        }
        // Recursive replace of curly brackets
        if ( cfg.getSubConfiguration(key).get().isMap() ){
            auto tmp = cfg.getSubConfiguration(key);
            replaceCurly( compConf, tmp );
            cfg.set(key,tmp);
        }

    }
    return;
}

fdb5::Config fdb5_configuration(const ComponentConfiguration& compConf) {
    auto fdb_configuration = compConf.parsedConfig().getSubConfiguration("config");
    replaceCurly(compConf,fdb_configuration);

    eckit::LocalConfiguration userConfig;
    if (fdb_configuration.has("userConfig")) {
        userConfig = fdb_configuration.getSubConfiguration("userConfig");
    }
    else {
        userConfig.set("useSubToc", true);
    }

    fdb5::Config fdb_config(fdb_configuration, userConfig);

    LOG_DEBUG_LIB(LibMultio) << "FDB5 Config = " << fdb_config << std::endl;

    return fdb_config;

}
}  // namespace


FDB5Sink::FDB5Sink(const ComponentConfiguration& compConf) : DataSink(compConf), fdb_{fdb5_configuration(compConf)} {
    LOG_DEBUG_LIB(LibMultio) << "Config = " << compConf.parsedConfig() << std::endl;
}

void FDB5Sink::write(eckit::message::Message msg) {
    LOG_DEBUG_LIB(LibMultio) << "FDB5Sink::write()" << std::endl;

    fdb_.archive(msg);
}

void FDB5Sink::flush() {
    LOG_DEBUG_LIB(LibMultio) << "FDB5Sink::flush()" << std::endl;

    fdb_.flush();
}

void FDB5Sink::print(std::ostream& os) const {
    os << "FDB5Sink()";
}

static DataSinkBuilder<FDB5Sink> FDB5SinkBuilder("fdb5");

}  // namespace multio::sink
