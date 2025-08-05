/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#pragma once

#include "multio/datamod/MarsMiscGeo.h"
#include "multio/mars2mars/rules/Rule.h"


namespace multio::mars2mars {
using namespace rules;
using namespace datamod;

const RuleList& allRules();

std::optional<MappingResult> applyMappings(datamod::MarsKeyValueSet&, datamod::MiscKeyValueSet&);

}  // namespace multio::mars2mars
