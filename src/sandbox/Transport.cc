/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Transport.h"

namespace multio {
namespace sandbox {

Transport::Transport(const eckit::Configuration& config) :
    name_(configure_value<std::string>(config, "name")) {}

Transport::~Transport() = default;

}  // namespace sandbox
}  // namespace multio
