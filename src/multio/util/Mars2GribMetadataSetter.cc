/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Mars2GribMetadataSetter.h"

namespace multio::util { 


//----------------------------------------------------------------------------------------------------------------------

Mars2GribMetadataSetter::Mars2GribMetadataSetter(message::BaseMetadata& map, bool nullOrMissingIsRemoval) :
    md_{map}, nullOrMissingIsRemoval_{nullOrMissingIsRemoval}  {}


void Mars2GribMetadataSetter::setValue(const std::string& key, const std::string& value) {
    md_.set(key, value);
}
void Mars2GribMetadataSetter::setValue(const std::string& key, long value) {
    md_.set(key, value);
}
void Mars2GribMetadataSetter::setValue(const std::string& key, double value) {
    md_.set(key, value);
}
void Mars2GribMetadataSetter::setValue(const std::string& key, metkit::mars2grib::NullOrMissing) {
    if (nullOrMissingIsRemoval_) {
        md_.erase(key);
    } else {
        md_.set(key, message::Null{});
    }
}

void Mars2GribMetadataSetter::print(std::ostream& os) const {
    os << "Mars2GribMetadataSetter{";
    os << md_;
    os << "Mars2GribMetadataSetter}";
}

}  // namespace multio::message
