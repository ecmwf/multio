/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#pragma once

#include <string>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/mpi/Comm.h"

namespace multio::distGrib1ToGrib2 {

eckit::LocalConfiguration loadOptionsFromYamlFile(const std::string& yamlFile);
eckit::LocalConfiguration loadAndBroadcastOptions(const std::string& yamlFile, const eckit::mpi::Comm& comm);
std::string debugOutputPrefix(const eckit::LocalConfiguration& options);

}  // namespace multio::distGrib1ToGrib2
