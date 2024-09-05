/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Philipp Geier

/// @date Jan 2024

#pragma once

#include "metkit/mars2grib/Mars2Grib.h"
#include "multio/message/BaseMetadata.h"

#include <memory>


namespace multio::util { 
//----------------------------------------------------------------------------------------------------------------------

class Mars2GribMetadataSetter: public metkit::mars2grib::KeySetter {
public:
    Mars2GribMetadataSetter(message::BaseMetadata& md, bool nullOrMissingIsRemoval=false);

    virtual ~Mars2GribMetadataSetter() = default;

    void setValue(const std::string& key, const std::string& value) override;
    void setValue(const std::string& key, long value) override;
    void setValue(const std::string& key, double value) override;

    void setValue(const std::string& key, metkit::mars2grib::NullOrMissing) override;

    void print(std::ostream& os) const override;

private:
    message::BaseMetadata& md_;
    bool nullOrMissingIsRemoval_;
};

//----------------------------------------------------------------------------------------------------------------------


}  // namespace multio::message
