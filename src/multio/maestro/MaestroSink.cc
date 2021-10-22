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
#include "unistd.h"

extern "C" {
#include <maestro.h>
}

#include "eckit/exception/Exceptions.h"
#include "eckit/mpi/Comm.h"

#include "multio/LibMultio.h"
#include "multio/util/ScopedTimer.h"


namespace multio {

MaestroSink::MaestroSink(const eckit::Configuration& config) : DataSink(config) {
    LOG_DEBUG_LIB(LibMultio) << "Config = " << config << std::endl;

    LOG_DEBUG_LIB(LibMultio) << *this << std::endl;

    eckit::Timing timing;
    {
        util::ScopedTimer timer{timing};
        auto componentName = std::string{::getenv("MSTRO_COMPONENT_NAME")} + " -- " +
                             std::to_string(eckit::mpi::comm().rank());
        ::sleep(15);
        mstro_status s = mstro_init(::getenv("MSTRO_WORKFLOW_NAME"),componentName.c_str(), 0);
        ASSERT(s == MSTRO_OK);
    }
    eckit::Log::info() << " MaestroSink: initialising Maestro has taken " << timing.elapsed_ << "s"
                       << std::endl;
}

MaestroSink::~MaestroSink() {

    eckit::Timing timing;
    {
        util::ScopedTimer timer{timing};

        std::for_each(begin(offered_cdos_), end(offered_cdos_), [](MaestroCdo& cdo) {
            LOG_DEBUG_LIB(LibMultio) << "Withdrawing CDO: " << cdo << std::endl;
            cdo.withdraw();
            cdo.dispose();
        });

        offered_cdos_.clear();

        mstro_finalize();
    }
    eckit::Log::info() << " MaestroSink: finalising Maestro has taken " << timing.elapsed_ << "s"
                       << std::endl;
//    statistics_.report(eckit::Log::info());
}

void MaestroSink::write(eckit::message::Message blob) {
    LOG_DEBUG_LIB(LibMultio) << "MaestroSink::write()" << std::endl;
//    eckit::AutoTiming timing(statistics_.timer_, statistics_.sinkWriteTiming_);

    MaestroMetadata md;

    eckit::message::StringSetter<MaestroMetadata> setter{md};
    blob.getMetadata(setter);

    std::ostringstream os;
    os << md;

    util::ScopedTimer timer{timing_};

    std::string name = "";
    {
//        eckit::AutoTiming timing(statistics_.timer_, statistics_.sinkNameTiming_);
        name = cdo_namer_.name(md);
    }
    LOG_DEBUG_LIB(LibMultio) << "Name: " << name << std::endl;

    {
//        eckit::AutoTiming timing(statistics_.timer_, statistics_.sinkCdoCreation_);
        offered_cdos_.emplace_back(name.c_str(), blob.data(), blob.length(), statistics_);
    }
    auto& cdo = offered_cdos_.back();

    LOG_DEBUG_LIB(LibMultio) << "metadata: " << md << std::endl;

    for (const auto& kw : md.keys()) {
//        eckit::AutoTiming timing(statistics_.timer_, statistics_.sinkAttributeTiming_);
        auto mkey = ".maestro.ecmwf." + kw;
        auto value = md.get<std::string>(kw);

        if (kw == "class" || kw == "domain" || kw == "expver" ||
            kw == "levtype" || kw == "stream" || kw == "type") {
            cdo.set_attribute(mkey.c_str(), value.c_str(), true);
        } else if (kw == "levelist" || kw == "number" || 
                   kw == "param" || kw == "step") {
            int64_t intvalue = std::stoi(value);
            cdo.set_attribute(mkey.c_str(), intvalue, true);
        } else if (kw == "time") {
            uint64_t intvalue = std::stoi(value);
            cdo.set_attribute(mkey.c_str(), intvalue, true);
        }
    }

    cdo.seal();               // Seal it after setting all attributes

    LOG_DEBUG_LIB(LibMultio) << " *** Offer cdo " << name.c_str() << std::endl;

    {
//        eckit::AutoTiming timing(statistics_.timer_, statistics_.sinkCdoOffer_);
        cdo.offer();               // Submit field
    }
}

void MaestroSink::flush() {
    {
        eckit::Log::info() << "MaestroSink::flush()" << std::endl;

        util::ScopedTimer timer{timing_};
    }
    eckit::Log::info() << " MaestroSink: CDO count = " << cdoCount_
                       << " -- writing the last step has taken " << timing_.elapsed_ << "s"
                       << std::endl;
    timing_ = eckit::Timing{}; // Resetting
}

void MaestroSink::print(std::ostream& os) const {
    os << "MaestroSink(libmaestro version " << mstro_version() << ")";
}

static DataSinkBuilder<MaestroSink> MaestroSinkBuilder("maestro");

}  // namespace multio
