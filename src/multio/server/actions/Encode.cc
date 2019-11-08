/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Encode.h"

#include <iostream>

#include "eccodes.h"
#include "eckit/config/Configuration.h"
#include "metkit/grib/GribHandle.h"

#include "multio/LibMultio.h"
#include "multio/server/GribTemplate.h"

namespace {
class GribEncoder : public metkit::grib::GribHandle {
    GribEncoder(const eckit::Buffer& buffer, bool copy = true) :
        metkit::grib::GribHandle{buffer, copy} {}

    void setValue(const std::string& key, long value) {
        CODES_CHECK(codes_set_long(raw(), key.c_str(), value), NULL);
    }

    void setValue(const std::string& key, double value) {
        CODES_CHECK(codes_set_double(raw(), key.c_str(), value), NULL);
    }

    void setValue(const std::string& key, const std::string& value) {
        size_t sz = value.size();
        CODES_CHECK(codes_set_string(raw(), key.c_str(), value.c_str(), &sz), NULL);
    }
};
}  // namespace

namespace multio {
namespace server {
namespace actions {

Encode::Encode(const eckit::Configuration& config) :
    Action{config},
    format_{config.getString("format")} {}

void Encode::execute(Message msg) const {
    if (format_ == "grib") {
        eckit::Log::debug<LibMultio>() << "*** Executing encoding: " << *this << std::endl;

        // const Message& grib_tmpl = GribTemplate::instance().get(msg.metadata().getString("cpref"),
        //                                                         msg.metadata().getBool("lspec"));

        // metkit::grib::GribHandle handle{msg.payload()};
        // templates_.emplace_back(new metkit::grib::GribHandle{msg.payload()});
    }

    if (next_) {  // May want to assert next_
        next_->execute(msg);
    }
}

void Encode::print(std::ostream& os) const {
    os << "Encode(format=" << format_ << ")";
}

static ActionBuilder<Encode> EncodeBuilder("Encode");

}  // namespace actions
}  // namespace server
}  // namespace multio
