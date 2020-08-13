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
#include "multio/action/GridInfo.h"

namespace multio {
namespace action {

class GribEncoder : public metkit::grib::GribHandle {
public:
    GribEncoder(codes_handle* handle);

    bool gridInfoReady(const std::string& subtype) const;
    bool setGridInfo(message::Message msg);
    void setOceanValues(const message::Metadata& md, const std::string& subtype);

    void setValue(const std::string& key, long value);
    void setValue(const std::string& key, double value);
    void setValue(const std::string& key, const std::string& value);
    void setValue(const std::string& key, const unsigned char* value);

    message::Message encodeLatitudes(const std::string& subtype);
    message::Message encodeLongitudes(const std::string& subtype);

private:

    void setCoordinateMetadata(const  message::Metadata& md);

    std::map<std::string, GridInfo> grids_{{"T grid", GridInfo{}},
                                           {"U grid", GridInfo{}},
                                           {"V grid", GridInfo{}},
                                           {"W grid", GridInfo{}},
                                           {"F grid", GridInfo{}}};

    std::set<std::string> coordSet_{"lat_T", "lon_T", "lat_U", "lon_U", "lat_V",
                                    "lon_V", "lat_W", "lon_W", "lat_F", "lon_F"};
};

}  // namespace action
}  // namespace multio

#endif
