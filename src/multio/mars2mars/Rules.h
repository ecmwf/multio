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

namespace dm = multio::datamod;

// Specific mappings

const RuleList& wmoUnitMapping();
const RuleList& fixIFSOutput();
const RuleList& mapDeprecatedGrib1ToGrib2();
const RuleList& waveBitsPerValue();

// Groups

const RuleList& mapBitsPerValue();
const RuleList& allRulesNoWMOMapping();

// All

const RuleList& allRules();

std::optional<MappingResult> applyMappings(const RuleList&, dm::MarsRecord&, dm::MiscRecord&);

}  // namespace multio::mars2mars
