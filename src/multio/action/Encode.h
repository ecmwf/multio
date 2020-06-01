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
/// @author Simon Smart
/// @author Tiago Quintino

/// @date Jan 2019

#ifndef multio_server_actions_Encode_H
#define multio_server_actions_Encode_H

#include <iosfwd>

#include "eccodes.h"

#include "metkit/grib/GribHandle.h"
#include "multio/action/Action.h"

namespace eckit {
class Configuration;
}

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


class Encode : public Action {
public:
    explicit Encode(const eckit::Configuration& config);

private:
    bool doExecute(message::Message& msg) const override;

    void print(std::ostream& os) const override;

    const std::string format_;

    const std::string gridType_;

    const std::unique_ptr<GribEncoder> encoder_ = nullptr;
};

}  // namespace action
}  // namespace multio

#endif
