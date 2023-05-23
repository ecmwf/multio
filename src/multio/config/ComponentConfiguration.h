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
                           const MultioConfiguration& multioConfig, ComponentTag tag = ComponentTag::Unrelated);

    eckit::LocalConfiguration& YAML();
    const eckit::LocalConfiguration& YAML() const;

    const MultioConfiguration& multioConfig() const;


    ComponentTag componentTag() const;
    void setComponentTag(ComponentTag);

    bool isServer() const;
    bool isClient() const;


    using SubComponentConfigurations
        = util::MappedContainer<std::vector<eckit::LocalConfiguration>, SubComponentIteratorMapper>;

    ComponentConfiguration subComponent(const std::string& subConfiguratinKey,
                                        ComponentTag tag = ComponentTag::Unrelated) const;
    SubComponentConfigurations subComponents(const std::string& subConfiguratinKey,
                                             ComponentTag tag = ComponentTag::Unrelated) const;

    ComponentConfiguration recast(const eckit::LocalConfiguration& componentYAMLConfig,
                                  ComponentTag tag = ComponentTag::Unrelated) const;
    ComponentConfiguration recast(ComponentTag tag = ComponentTag::Unrelated) const;

private:
    eckit::LocalConfiguration componentConf_;
    // Put in reference wrapper to enable default copy/move construction & assignment
    std::reference_wrapper<const MultioConfiguration> multioConf_;
    ComponentTag componentTag_;

    friend class SubComponentIteratorMapper;
};


class SubComponentIteratorMapper {
public:
    SubComponentIteratorMapper(const ComponentConfiguration& compConf, ComponentTag tag = ComponentTag::Unrelated);
    SubComponentIteratorMapper(ComponentConfiguration&& compConf, ComponentTag tag = ComponentTag::Unrelated);

    ComponentConfiguration operator()(const eckit::LocalConfiguration& componentYAMLConfig) const;

private:
    ComponentConfiguration compConf_;
    ComponentTag tag_;
};

}  // namespace multio::config
