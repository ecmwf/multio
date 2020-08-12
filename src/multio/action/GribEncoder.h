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

#ifndef multio_server_actions_GribEncoder_H
#define multio_server_actions_GribEncoder_H

#include "eccodes.h"

#include "metkit/grib/GribHandle.h"
#include "multio/message/Message.h"

namespace multio {
namespace action {

class GribEncoder : public metkit::grib::GribHandle {
public:
    GribEncoder(codes_handle* handle);

    void setOceanValues(const message::Metadata& md);

    void setValue(const std::string& key, long value);
    void setValue(const std::string& key, double value);
    void setValue(const std::string& key, const std::string& value);
};

}  // namespace action
}  // namespace multio

#endif
