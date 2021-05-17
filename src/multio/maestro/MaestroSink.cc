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

#include "eckit/exception/Exceptions.h"
#include "eckit/mpi/Comm.h"
#include "eckit/value/Value.h"

#include "multio/LibMultio.h"
#include "multio/util/ScopedTimer.h"

namespace multio {

namespace  {
class Metadata : protected eckit::LocalConfiguration {
    void print(std::ostream& os) const {
        os << *root_ << std::endl;
    }
    friend std::ostream& operator<<(std::ostream& os, const Metadata& md) {
        md.print(os);
        return os;
    }

public:
    template <typename T>
    void setValue(const std::string& key, const T& value) {
        set(key, value);
    }

    template <typename T>
    T get(const std::string& key) {
        T value;
        eckit::LocalConfiguration::get(key, value);
        return value;
    }

    std::vector<std::string> keys() {
        return eckit::LocalConfiguration::keys();
    }
};
}

MaestroSink::MaestroSink(const eckit::Configuration& config) : DataSink(config) {
    LOG_DEBUG_LIB(LibMultio) << "Config = " << config << std::endl;

    LOG_DEBUG_LIB(LibMultio) << *this << std::endl;

    eckit::Timing timing;
    {
        util::ScopedTimer timer{timing};
        auto componentName = std::string{::getenv("MSTRO_COMPONENT_NAME")} + " -- " +
                             std::to_string(eckit::mpi::comm().rank());
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

        ::sleep(60);

        std::for_each(begin(offered_cdos_), end(offered_cdos_), [](mstro_cdo cdo) {
            mstro_cdo_withdraw(cdo);
            mstro_cdo_dispose(cdo);
        });

        offered_cdos_.clear();

        mstro_finalize();
    }
    eckit::Log::info() << " MaestroSink: finalising Maestro has taken " << timing.elapsed_ << "s"
                       << std::endl;
}

void MaestroSink::write(eckit::message::Message blob) {
    auto name = std::string("multio-hammer-cdo--") + std::to_string(cdoCount_++);

    LOG_DEBUG_LIB(LibMultio) << "MaestroSink::write()" << std::endl;

    Metadata md;

    eckit::message::TypedSetter<Metadata> setter{md};
    blob.getMetadata(setter);

    std::ostringstream os;
    os << md;

    util::ScopedTimer timer{timing_};

    mstro_cdo cdo = nullptr;
    mstro_status s = mstro_cdo_declare(name.c_str(), MSTRO_ATTR_DEFAULT, &cdo);

    auto buf = static_cast<void*>(new char[blob.length()]);
    uint64_t sz = blob.length();

    ::memcpy(buf, blob.data(), sz);

    s = mstro_cdo_attribute_set(cdo, ".maestro.core.cdo.raw-ptr", buf, false);
    s = mstro_cdo_attribute_set(cdo, ".maestro.core.cdo.scope.local-size", &sz, true);

    eckit::Log::info() << " *** MaestroSink *** buffer " << buf << " with size " << sz << std::endl;

    LOG_DEBUG_LIB(LibMultio) << "metadata: " << md << std::endl;

    for (const auto& kw : md.keys()) {
        auto value = md.get<std::string>(kw);

        auto mkey = ".maestro.ecmwf." + kw;
        auto intvalue = std::stoi(value);
        auto mvalue = static_cast<void*>(&intvalue);
        s = mstro_cdo_attribute_set(cdo, mkey.c_str(), &mvalue, true);
    }

    s = mstro_cdo_seal(cdo);  // Seal it after setting all attributes

    eckit::Log::info() << " *** Offer cdo " << name.c_str() << std::endl;

    s = mstro_cdo_offer(cdo);  // Submit field

    offered_cdos_.push_back(cdo);
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
