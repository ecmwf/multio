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

extern "C" {
#include <maestro.h>
}

#include "eckit/exception/Exceptions.h"
#include "eckit/mpi/Comm.h"

#include "multio/LibMultio.h"

#include "multio/util/ScopedTimer.h"


namespace multio {

MaestroSink::MaestroSink(const ComponentConfiguration& compConf) : multio::sink::DataSink(compConf) {
    LOG_DEBUG_LIB(LibMultio) << "Config = " << compConf.parsedConfig() << std::endl;

    LOG_DEBUG_LIB(LibMultio) << *this << std::endl;
    readyCdoEnabled_ = compConf.parsedConfig().getBool("ready-cdo", true);

    eckit::Timing timing;
    {
        util::ScopedTimer timer{timing};
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
    eckit::Log::info() << " MaestroSink: initialising Maestro has taken " << timing.elapsed_ << "s" << std::endl;
}

MaestroSink::~MaestroSink() {

    eckit::Timing timing;
    {
        util::ScopedTimer timer{timing};
        if (readyCdoEnabled_)
            readyCdo_.dispose();
        mstro_finalize();
    }
    eckit::Log::info() << " MaestroSink: finalising Maestro has taken " << timing.elapsed_ << "s" << std::endl;

    statistics_.report(eckit::Log::info());
}

void MaestroSink::write(eckit::message::Message blob) {
    LOG_DEBUG_LIB(LibMultio) << "MaestroSink::write()" << std::endl;
    util::ScopedTiming timing(statistics_.sinkWriteTimer_, statistics_.sinkWriteTiming_);

    MaestroMetadata md;

    eckit::message::StringSetter<MaestroMetadata> setter{md};
    blob.getMetadata(setter);

    std::ostringstream os;
    os << md;

    util::ScopedTimer timer{timing_};

    std::string name = "";
    {
        util::ScopedTiming timing(statistics_.sinkNameTimer_, statistics_.sinkNameTiming_);
        name = cdo_namer_.name(md);
    }
    LOG_DEBUG_LIB(LibMultio) << "Name: " << name << std::endl;

    {
        util::ScopedTiming timing(statistics_.sinkCdoCreationTimer_, statistics_.sinkCdoCreationTiming_);
        offered_cdos_.emplace_back(name.c_str(), blob.data(), blob.length());
    }
    auto& cdo = offered_cdos_.back();

    LOG_DEBUG_LIB(LibMultio) << "metadata: " << md << std::endl;

    for (const auto& kw : md.keys()) {
        util::ScopedTiming timing(statistics_.sinkAttributeTimer_, statistics_.sinkAttributeTiming_);
        auto mkey = ".maestro.ecmwf." + kw;
        auto value = md.get<std::string>(kw);

        if (kw == "class" || kw == "domain" || kw == "expver" || kw == "levtype" || kw == "stream" || kw == "type") {
            cdo.set_attribute(mkey.c_str(), value.c_str(), true);
        }
        else if (kw == "levelist" || kw == "number" || kw == "param" || kw == "step") {
            int64_t intvalue = std::stoi(value);
            cdo.set_attribute(mkey.c_str(), intvalue, true);
        }
        else if (kw == "time") {
            uint64_t intvalue = std::stoi(value);
            cdo.set_attribute(mkey.c_str(), intvalue, true);
        }
    }

    cdo.seal();  // Seal it after setting all attributes

    LOG_DEBUG_LIB(LibMultio) << " *** Offer cdo " << name.c_str() << std::endl;

    {
        util::ScopedTiming timing(statistics_.sinkCdoOfferTimer_, statistics_.sinkCdoOfferTiming_);
        cdo.offer();  // Submit field
        ++cdoCount_;
    }
}

void MaestroSink::flush() {
    {
        eckit::Log::info() << "MaestroSink::flush()" << std::endl;

        util::ScopedTimer timer{timing_};

        std::for_each(begin(offered_cdos_), end(offered_cdos_), [](MaestroCdo& cdo) {
            LOG_DEBUG_LIB(LibMultio) << "Withdrawing CDO: " << cdo << std::endl;
            cdo.withdraw();
            cdo.dispose();
        });

        offered_cdos_.clear();
    }
    eckit::Log::info() << " MaestroSink: CDO count = " << cdoCount_ << " -- writing the last step has taken "
                       << timing_.elapsed_ << "s" << std::endl;
    timing_ = eckit::Timing{};  // Resetting
}

void MaestroSink::print(std::ostream& os) const {
    os << "MaestroSink(libmaestro version " << mstro_version() << ")";
}

static sink::DataSinkBuilder<MaestroSink> MaestroSinkBuilder("maestro");

}  // namespace multio
