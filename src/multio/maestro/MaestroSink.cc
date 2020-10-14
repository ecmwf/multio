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
#include <string>
#include <thread>

#include "eckit/exception/Exceptions.h"

#include "multio/LibMultio.h"

namespace multio {

namespace  {
class Metadata {
    eckit::LocalConfiguration local_;
    void print(std::ostream& os) const {
        os << local_ << std::endl;
    }
    friend std::ostream& operator<<(std::ostream& os, const Metadata& md) {
        md.print(os);
        return os;
    }

public:
    template <typename T>
    void setValue(const std::string& key, const T& value) {
        local_.set(key, value);
    }

    template <typename T>
    T get(const std::string& key) {
        T value;
        local_.get(key, value);
        return value;
    }

    std::vector<std::string> keys() {
        return local_.keys();
    }
};
}

MaestroSink::MaestroSink(const eckit::Configuration& config) : DataSink(config) {
    LOG_DEBUG_LIB(LibMultio) << "Config = " << config << std::endl;

    LOG_DEBUG_LIB(LibMultio) << *this << std::endl;

    eckit::Timing timing;
    {
        action::ScopedTimer timer{timing};
        mstro_status s =
            mstro_init(::getenv("MSTRO_WORKFLOW_NAME"), ::getenv("MSTRO_COMPONENT_NAME"), 0);
        ASSERT(s == MSTRO_OK);
    }
    eckit::Log::info() << " MaestroSink: initialising Maestro has taken " << timing.elapsed_ << "s"
                       << std::endl;
}

MaestroSink::~MaestroSink() {
    eckit::Timing timing;
    {
        action::ScopedTimer timer{timing};
        mstro_finalize();
    }
    eckit::Log::info() << " MaestroSink: finalising Maestro has taken " << timing.elapsed_ << "s"
                       << std::endl;
}

void MaestroSink::write(eckit::message::Message blob) {

    auto name = std::to_string(std::hash<std::thread::id>{}(std::this_thread::get_id())) +
                "-" + std::to_string(cdoCount_++);

    LOG_DEBUG_LIB(LibMultio) << "MaestroSink::write()" << std::endl;

    action::ScopedTimer timer{timing_};

    mstro_cdo cdo = nullptr;
    mstro_status s = mstro_cdo_declare(name.c_str(), MSTRO_ATTR_DEFAULT, &cdo);

    const void *cbuf = blob.data();
    void * buf = const_cast<void*>(cbuf);
    s = mstro_cdo_attribute_set(cdo, ".maestro.core.cdo.raw-ptr", buf);

    auto sz = blob.length();
    s = mstro_cdo_attribute_set(cdo, ".maestro.core.cdo.scope.local-size", &sz);

    Metadata md;

    eckit::message::TypedSetter<Metadata> setter{md};
    blob.getMetadata(setter);

    LOG_DEBUG_LIB(LibMultio) << "metadata: " << md << std::endl;

    for (const auto& kw : md.keys()) {
        auto value = md.get<std::string>(kw);

        auto mkey = ".maestro.ecmwf." + kw;
        auto mvalue = const_cast<char*>(value.c_str());

        s = mstro_cdo_attribute_set(cdo, mkey.c_str(), mvalue);
    }

    s = mstro_cdo_declaration_seal(cdo); // Seal it after setting all attributes

    s = mstro_cdo_offer(cdo); // Submit field

    offered_cdos_.push_back(cdo);
}

void MaestroSink::flush() {
    {
        action::ScopedTimer timer{timing_};
        LOG_DEBUG_LIB(LibMultio) << "MaestroSink::flush()" << std::endl;

        std::for_each(begin(offered_cdos_), end(offered_cdos_), [](mstro_cdo cdo) {
            mstro_status s = mstro_cdo_withdraw(cdo);
            s = mstro_cdo_dispose(cdo);
        });

        offered_cdos_.clear();
    }
    eckit::Log::info() << " MaestroSink: CDO count = " << cdoCount_
                       << " -- writing the last step has taken " << timing_.elapsed_ << "s"
                       << std::endl;
    timing_ = eckit::Timing{}; // Resetting
}

void MaestroSink::print(std::ostream& os) const {
    os << "MaestroSink using libmaestro version: " << mstro_version();
}

static DataSinkBuilder<MaestroSink> MaestroSinkBuilder("maestro");

}  // namespace multio
