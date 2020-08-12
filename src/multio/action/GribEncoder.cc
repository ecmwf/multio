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

/// @date Aug 2020

#include "GribEncoder.h"

#include <iostream>

#include "eckit/log/Log.h"
#include "multio/LibMultio.h"

namespace multio {
namespace action {

GribEncoder::GribEncoder(codes_handle* handle) : metkit::grib::GribHandle{handle} {}

void GribEncoder::setOceanValues(const message::Metadata& md) {
    // setCommonMetadata
    setValue("expver", "xxxx");
    setValue("class", "rd");
    setValue("stream", "oper");
    setValue("type", "fc");
    setValue("levtype", static_cast<long>(168));
    setValue("step", md.getLong("step"));
    setValue("level", md.getLong("level"));

    // TODO: Nemo should set this at the beginning of the run
    setValue("date", 20170906l);

    // setDomainDimensions
    setValue("numberOfDataPoints", md.getLong("globalSize"));
    setValue("numberOfValues", md.getLong("globalSize"));

    // Setting parameter ID
    setValue("param", md.getLong("param"));
}

void GribEncoder::setValue(const std::string& key, long value) {
    eckit::Log::debug<multio::LibMultio>() << "Setting value for key " << key << std::endl;
    CODES_CHECK(codes_set_long(raw(), key.c_str(), value), NULL);
}

void GribEncoder::setValue(const std::string& key, double value) {
    eckit::Log::debug<multio::LibMultio>() << "Setting value for key " << key << std::endl;
    CODES_CHECK(codes_set_double(raw(), key.c_str(), value), NULL);
}

void GribEncoder::setValue(const std::string& key, const std::string& value) {
    eckit::Log::debug<multio::LibMultio>()
        << "Setting value " << value << " for key " << key << std::endl;
    size_t sz = value.size();
    CODES_CHECK(codes_set_string(raw(), key.c_str(), value.c_str(), &sz), NULL);
}

}  // namespace action
}  // namespace multio
