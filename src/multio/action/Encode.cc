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
#include "eckit/exception/Exceptions.h"
#include "eckit/io/StdFile.h"

#include "metkit/grib/GribDataBlob.h"
#include "metkit/grib/GribHandle.h"

#include "multio/library/LibMultio.h"

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
        eckit::Log::debug<multio::LibMultio>() << "Setting value for key " << key << std::endl;
        CODES_CHECK(codes_set_long(raw(), key.c_str(), value), NULL);
    }

    void setValue(const std::string& key, double value) {
        eckit::Log::debug<multio::LibMultio>() << "Setting value for key " << key << std::endl;
        CODES_CHECK(codes_set_double(raw(), key.c_str(), value), NULL);
    }

    void setValue(const std::string& key, const std::string& value) {
        eckit::Log::debug<multio::LibMultio>() << "Setting value for key " << key << std::endl;
        size_t sz = value.size();
        CODES_CHECK(codes_set_string(raw(), key.c_str(), value.c_str(), &sz), NULL);
    }
};
}  // namespace

namespace multio {
namespace action {

using message::Message;
using message::Peer;

Encode::Encode(const eckit::Configuration& config) :
    Action{config},
    format_{config.getString("format")},
    template_{config.has("template") ? config.getString("template") : ""} {}

bool Encode::doExecute(Message& msg) const {
    ScopedTimer timer{timing_};

    if (format_ == "grib") {
        eckit::Log::debug<LibMultio>() << "*** Executing encoding: " << *this << std::endl;

        eckit::AutoStdFile fin{configuration_path() + template_};
        int err;
        GribEncoder encoder{codes_handle_new_from_file(nullptr, fin, PRODUCT_GRIB, &err)};

        const auto& md = msg.metadata();

        // setCommonMetadata
        encoder.setValue("expver", "xxxx");
        encoder.setValue("class", "rd");
        encoder.setValue("stream", "oper");
        encoder.setValue("type", "fc");
        encoder.setValue("levtype", static_cast<long>(168));
        encoder.setValue("step", md.getLong("istep"));
        encoder.setValue("level", md.getLong("ilevg"));

        // setDomainDimensions
        encoder.setValue("numberOfDataPoints", md.getLong("isizeg"));
        encoder.setValue("numberOfValues", md.getLong("isizeg"));

        // Setting parameter ID
        encoder.setValue("param", md.getLong("param"));

        // Setting field values
        auto beg = reinterpret_cast<const double*>(msg.payload().data());
        encoder.setDataValues(beg, msg.globalSize());

        eckit::Buffer buf{encoder.message()->length()};
        encoder.write(buf);
        msg =
            Message{Message::Header{Message::Tag::Grib, Peer{"", 0}, Peer{"", 0}}, std::move(buf)};
    }
    else if (format_ == "none") {
        ;  // leave message in raw binary format
    }
    else {
        throw eckit::SeriousBug("Encoding format <" + format_ + "> is not supported");
    }

    return true;
}

void Encode::print(std::ostream& os) const {
    os << "Encode(format=" << format_ << ")";
}

static ActionBuilder<Encode> EncodeBuilder("Encode");

}  // namespace action
}  // namespace multio
