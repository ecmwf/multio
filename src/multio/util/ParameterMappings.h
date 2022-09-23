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

#include <functional>
#include <unordered_map>
#include "eckit/config/LocalConfiguration.h"
#include "multio/message/ParameterMapping.h"

namespace multio {
namespace util {

class GlobalConfCtx;  // Forward declaration

class ParameterMappings {
public:
    ParameterMappings(std::reference_wrapper<const GlobalConfCtx> globalConfCtx);

    std::pair<std::string, std::shared_ptr<std::vector<message::ParameterMapping>>> getMappings(const std::string& mapping) const;

private:
    std::reference_wrapper<const GlobalConfCtx> globalConfCtx_;
    eckit::LocalConfiguration configs_;
    mutable std::unordered_map<std::string, std::shared_ptr<std::vector<message::ParameterMapping>>> mappings_;
};
}  // namespace util
}  // namespace multio

#endif
