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

/// @date Aug 2022

#ifndef multio_util_ParameterMappings_H
#define multio_util_ParameterMappings_H

#include "eckit/config/LocalConfiguration.h"

#include "multio/message/ParameterMapping.h"

#include <functional>
#include <unordered_map>


namespace multio {
namespace util {

class GlobalConfCtx;  // Forward declaration

class ParameterMappings {
public:
    ParameterMappings(const GlobalConfCtx& globalConfCtx);

    const std::vector<message::ParameterMapping>& getMappings(const std::string& mapping) const;

private:
    const GlobalConfCtx& globalConfCtx_;
    eckit::LocalConfiguration configs_;
    mutable std::unordered_map<std::string, std::vector<message::ParameterMapping>> mappings_;
};
}  // namespace util
}  // namespace multio

#endif
