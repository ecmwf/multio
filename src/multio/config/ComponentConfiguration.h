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

// Explicitly create specializations for compile time checks
class ServerConfiguration;
class ClientConfiguration;



class ComponentConfiguration {
public:
    ComponentConfiguration(const eckit::PathName& fileName = configuration_file_name(),
                         LocalPeerTag clientOrServer = LocalPeerTag::Client,
                         ComponentTag tag = ComponentTag::Unrelated);

    ComponentConfiguration(const eckit::PathName& pathName, const eckit::PathName& fileName,
                         LocalPeerTag clientOrServer = LocalPeerTag::Client,
                         ComponentTag tag = ComponentTag::Unrelated);

    ComponentConfiguration(const eckit::LocalConfiguration& componentYAMLConfig, const eckit::PathName& pathName,
                         const eckit::PathName& fileName, LocalPeerTag clientOrServer = LocalPeerTag::Client,
                         ComponentTag tag = ComponentTag::Unrelated);

    ComponentConfiguration(const eckit::LocalConfiguration& componentYAMLConfig, const eckit::LocalConfiguration& globalYAML,
                         const eckit::PathName& pathName, const eckit::PathName& fileName,
                         LocalPeerTag clientOrServer = LocalPeerTag::Client,
                         ComponentTag tag = ComponentTag::Unrelated);

    eckit::LocalConfiguration& YAML();
    const eckit::LocalConfiguration& YAML() const;
    
    MultioConfiguration& multioConfig();
    const MultioConfiguration& multioConfig() const;
    

    ComponentTag componentTag() const;
    ComponentConfiguration& setComponentTag(ComponentTag);

    bool isServer() const;
    bool isClient() const;
    ComponentConfiguration& tagServer();
    ComponentConfiguration& tagClient();


    using SubComponentConfigurations = util::MappedContainer<std::vector<eckit::LocalConfiguration>, SubComponentIteratorMapper>;

    ComponentConfiguration subComponent(const std::string& subConfiguratinKey,
                                    ComponentTag tag = ComponentTag::Unrelated) const;
    SubComponentConfigurations subComponents(const std::string& subConfiguratinKey,
                                         ComponentTag tag = ComponentTag::Unrelated) const;
                                         
    ComponentConfiguration recast(const eckit::LocalConfiguration& componentYAMLConfig,
                                ComponentTag tag = ComponentTag::Unrelated) const;
    ComponentConfiguration recast(ComponentTag tag = ComponentTag::Unrelated) const;

protected:
    ComponentConfiguration(const eckit::LocalConfiguration& componentYAMLConfig, std::shared_ptr<MultioConfiguration> multioConf,
                         ComponentTag tag = ComponentTag::Unrelated);


private:
    eckit::LocalConfiguration componentConf_;
    std::shared_ptr<MultioConfiguration> multioConf_;
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


namespace {
ComponentConfiguration throwRecast_(const ComponentConfiguration& compConf, const std::string& key) {
    return compConf.recast(([&]() {
        try {
            return compConf.multioConfig().YAML().getSubConfiguration(key);
        }
        catch (...) {
            std::ostringstream oss;
            oss << "Configuration '" << key << "' not found in configuration file " << compConf.multioConfig().fileName();
            std::throw_with_nested(eckit::UserError(oss.str(), Here()));
        }
    })());
}
}  // namespace


class ClientConfiguration : public ComponentConfiguration {
public:
    ClientConfiguration(const ComponentConfiguration& otherBase, const std::string& key = "client") :
        ComponentConfiguration(((!otherBase.YAML().has("client") && otherBase.YAML().has("plans"))
                                  ? ComponentConfiguration(otherBase)
                                  : throwRecast_(otherBase, key))
                                 .tagClient()) {}
};


class ServerConfiguration : public ComponentConfiguration {
public:
    ServerConfiguration(const ComponentConfiguration& otherBase, const std::string& serverName = "server") :
        ComponentConfiguration(throwRecast_(otherBase, serverName).tagServer()) {}
};


}  // namespace multio::config
