/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Field.h"

#include <sstream>

#include "eckit/io/Buffer.h"
#include "eckit/parser/JSON.h"

namespace multio {
namespace sandbox {

namespace {
std::string pack_configuration(const eckit::LocalConfiguration& config) {
    std::stringstream ss;
    eckit::JSON json(ss);
    json << config;
    return ss.str();
}
}  // namespace

template <typename T, typename = typename std::enable_if<std::is_arithmetic<T>::value, T>::type>
eckit::Buffer Field<T>::to_payload() const {
}

}  // namespace sandbox
}  // namespace multio
