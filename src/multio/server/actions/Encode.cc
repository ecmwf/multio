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
#include "eckit/io/StdFile.h"

#include "metkit/grib/GribHandle.h"

#include "multio/LibMultio.h"
#include "multio/server/GribTemplate.h"

namespace {
eckit::PathName configuration_path() {
    eckit::PathName base = (::getenv("MULTIO_SERVER_PATH"))
                               ? eckit::PathName{::getenv("MULTIO_SERVER_PATH")}
                               : eckit::PathName{""};

    return base + "/configs/";
}

class GribEncoder : public metkit::grib::GribHandle {
public:
    GribEncoder(codes_handle* handle) : metkit::grib::GribHandle{handle} {}

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
    format_{config.getString("format")},
    template_{config.has("template") ? config.getString("template") : ""} {}

void Encode::execute(Message msg) const {
    if (format_ == "grib") {
        eckit::Log::debug<LibMultio>() << "*** Executing encoding: " << *this << std::endl;

        eckit::AutoStdFile fin{configuration_path() + template_};
        int err;
        GribEncoder encoder{codes_handle_new_from_file(nullptr, fin, PRODUCT_GRIB, &err)};

        const auto& md = msg.metadata();

        // setCommonMetadata
        encoder.setValue("expver", md.getString("expver"));
        encoder.setValue("class", md.getString("class"));
        encoder.setValue("stream", md.getString("stream"));
        encoder.setValue("type", md.getString("type"));
        encoder.setValue("levtype", md.getLong("levtype"));
        encoder.setValue("step", md.getLong("step"));
        encoder.setValue("level", md.getLong("level"));

        // setDomainDimensions
        encoder.setValue("numberOfDataPoints", md.getLong("globalSize"));
        encoder.setValue("numberOfValues", md.getLong("globalSize"));

        encoder.setValue("param", md.getLong("param"));

        auto beg = reinterpret_cast<const double*>(msg.payload().data());
        encoder.setDataValues(beg, msg.globalSize());
    }
    else if (format_ == "none") {
        ; // leave in raw binary format
    }
    else {
        throw eckit::SeriousBug("Encoding format <" + format_ + "> is not supported");
    }

    if (next_) {  // May want to assert next_
        next_->execute(msg);
    }
}

        void Encode::print(std::ostream & os) const { os << "Encode(format=" << format_ << ")"; }

        static ActionBuilder<Encode> EncodeBuilder("Encode");

    }  // namespace actions
}  // namespace server
}  // namespace multio
