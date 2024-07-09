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

#include "multio/maestro/MaestroSink.h"

#include <algorithm>
#include <cstring>
#include <string>
#include <thread>
#include <type_traits>

extern "C" {
#include <maestro.h>
}

#include "eckit/exception/Exceptions.h"
#include "eckit/mpi/Comm.h"

#include "multio/LibMultio.h"

#include "multio/util/Timing.h"

namespace {
static void setStringValue(multio::MaestroCdo& cdo, const std::string& key, const std::string& value) {
    cdo.set_attribute(key.c_str(), value.c_str(), true);
}

static void setInt64Value(multio::MaestroCdo& cdo, const std::string& key, const std::string& value) {
    int64_t intvalue = std::stoi(value);
    cdo.set_attribute(key.c_str(), &intvalue, true);
}


using MaestroKeySetter = std::add_pointer<void(multio::MaestroCdo&, const std::string&, const std::string&)>::type;

static const std::map<std::string, MaestroKeySetter> cdoValueSetters{
    {"class", &setStringValue},   {"expver", &setStringValue},  {"stream", &setStringValue},
    {"date", &setStringValue},    {"time", &setStringValue},    {"domain", &setStringValue},
    {"type", &setStringValue},    {"levtype", &setStringValue}, {"step", &setInt64Value},
    {"anoffset", &setInt64Value}, {"levelist", &setInt64Value}, {"param", &setInt64Value},
    // # Additional attributes (D340.2.2.3). These keys will be put back once version 2.2.3 is released.
    //{"experiment", &setStringValue},
    //{"activity", &setStringValue},
    //{"generation", &setInt64Value},
    //{"realization", &setInt64Value},
    //{"model", &setStringValue},
    //{"resolution", &setStringValue},
    //{"frequency", &setStringValue},
    //{"direction", &setStringValue},
};
}  // namespace

namespace multio {

MaestroSink::MaestroSink(const ComponentConfiguration& compConf) : multio::sink::DataSink(compConf) {
    LOG_DEBUG_LIB(LibMultio) << "Config = " << compConf.parsedConfig() << std::endl;

    LOG_DEBUG_LIB(LibMultio) << *this << std::endl;
    readyCdoEnabled_ = compConf.parsedConfig().getBool("ready-cdo", true);

    schemaName_ = compConf.parsedConfig().getString("schema", ".maestro.ecmwf.");

    util::Timing<> timing;
    {
        util::ScopedTiming timer{timing};
        ASSERT(!::getenv("MSTRO_COMPONENT_NAME"));
        ASSERT(::getenv("COMPONENT_NAME"));
        auto componentName
            = std::string{::getenv("COMPONENT_NAME")} + " -- " + std::to_string(eckit::mpi::comm().rank());
        mstro_status s = mstro_init(::getenv("MSTRO_WORKFLOW_NAME"), componentName.c_str(), 0);
        ASSERT(s == MSTRO_OK);

        auto component = std::string{::getenv("COMPONENT_NAME")};
        auto delimiter = std::string{" - "};
        auto number
            = std::atoi(component.substr(component.find(delimiter) + delimiter.length(), component.length()).c_str());
        if (readyCdoEnabled_) {
            auto readyCdoName = std::string{"READY - "} + std::to_string(number);
            readyCdo_ = MaestroCdo{readyCdoName};
            eckit::Log::info() << "Waiting for CDO [" << readyCdo_ << "] to be ready." << std::endl;
            readyCdo_.require();
            eckit::mpi::comm().barrier();
            readyCdo_.demand();
        }
    }
    timing.process();
    eckit::Log::info() << " MaestroSink: initialising Maestro has taken " << timing.elapsedTimeSeconds() << "s"
                       << std::endl;
}

MaestroSink::~MaestroSink() {

    util::Timing<> timing;
    {
        util::ScopedTiming timer{timing};
        if (readyCdoEnabled_)
            readyCdo_.dispose();
        mstro_finalize();
    }
    timing.process();
    eckit::Log::info() << " MaestroSink: finalising Maestro has taken " << timing.elapsedTimeSeconds() << "s"
                       << std::endl;

    statistics_.report(eckit::Log::info());
}

void MaestroSink::write(eckit::message::Message blob) {
    LOG_DEBUG_LIB(LibMultio) << "MaestroSink::write()" << std::endl;
    util::ScopedTiming timing(statistics_.sinkWriteTiming_);

    MaestroMetadata md;

    eckit::message::StringSetter<MaestroMetadata> setter{md};
    blob.getMetadata(setter);

    std::ostringstream os;
    os << md;

    util::ScopedTiming timer{timing_};

    std::string name = "";
    {
        util::ScopedTiming timing(statistics_.sinkNameTiming_);
        name = cdo_namer_.name(md);
    }
    LOG_DEBUG_LIB(LibMultio) << "Name: " << name << std::endl;

    {
        util::ScopedTiming timing(statistics_.sinkCdoCreationTiming_);
        offered_cdos_.emplace_back(name.c_str(), blob.data(), blob.length());
    }
    auto& cdo = offered_cdos_.back();

    LOG_DEBUG_LIB(LibMultio) << "metadata: " << md << std::endl;

    for (const auto& kw : md.keys()) {
        util::ScopedTiming timing(statistics_.sinkAttributeTiming_);
        auto mkey = schemaName_ + kw;
        auto value = md.get<std::string>(kw);

        if (cdoValueSetters.count(kw) != 0) {
            std::invoke(cdoValueSetters.at(kw), cdo, mkey, value);
        }
    }

    cdo.seal();  // Seal it after setting all attributes

    LOG_DEBUG_LIB(LibMultio) << " *** Offer cdo " << name.c_str() << std::endl;

    {
        util::ScopedTiming timing(statistics_.sinkCdoOfferTiming_);
        cdo.offer();  // Submit field
        ++cdoCount_;
    }
}

void MaestroSink::flush() {
    {
        eckit::Log::info() << "MaestroSink::flush()" << std::endl;

        util::ScopedTiming timer{timing_};

        std::for_each(begin(offered_cdos_), end(offered_cdos_), [](MaestroCdo& cdo) {
            LOG_DEBUG_LIB(LibMultio) << "Withdrawing CDO: " << cdo << std::endl;
            cdo.withdraw();
            cdo.dispose();
        });

        offered_cdos_.clear();
    }
    timing_.process();
    eckit::Log::info() << " MaestroSink: CDO count = " << cdoCount_ << " -- writing the last step has taken "
                       << timing_.elapsedTimeSeconds() << "s" << std::endl;
    timing_ = util::Timing<>{};  // Resetting
}

void MaestroSink::print(std::ostream& os) const {
    os << "MaestroSink(libmaestro version " << mstro_version() << ")";
}

static sink::DataSinkBuilder<MaestroSink> MaestroSinkBuilder("maestro");

}  // namespace multio
