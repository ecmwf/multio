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

#pragma once

#include "multio/config/MultioConfiguration.h"

#include "multio/util/IteratorMapper.h"
#include "multio/util/Substitution.h"

#include <functional>
#include <tuple>
#include <unordered_map>


namespace multio::config {

class SubComponentIteratorMapper;


class ComponentConfiguration {
public:
    ComponentConfiguration(const eckit::LocalConfiguration& componentYAMLConfig,
                           const MultioConfiguration& multioConfig);

    eckit::LocalConfiguration& parsedConfig();
    const eckit::LocalConfiguration& parsedConfig() const;

    const MultioConfiguration& multioConfig() const;

    using SubComponentConfigurations
        = util::MappedContainer<std::vector<eckit::LocalConfiguration>, SubComponentIteratorMapper>;

    ComponentConfiguration subComponent(const std::string& subConfiguratinKey) const;
    SubComponentConfigurations subComponents(const std::string& subConfiguratinKey) const;

private:
    eckit::LocalConfiguration componentConf_;
    // Put in reference wrapper to enable default copy/move construction & assignment
    std::reference_wrapper<const MultioConfiguration> multioConf_;

    friend class SubComponentIteratorMapper;
};


class SubComponentIteratorMapper {
public:
    SubComponentIteratorMapper(const ComponentConfiguration& compConf);
    SubComponentIteratorMapper(ComponentConfiguration&& compConf);

    ComponentConfiguration operator()(const eckit::LocalConfiguration& componentYAMLConfig) const;

private:
    ComponentConfiguration compConf_;
};

}  // namespace multio::config
