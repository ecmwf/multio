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

#include "multio/config/ConfigurationPath.h"
#include "multio/config/MetadataMappings.h"

#include "eckit/config/LocalConfiguration.h"

#include "eckit/utils/Translator.h"


#include <functional>
#include <optional>
#include <tuple>
#include <unordered_map>

//=============================================================================

namespace multio::config {

enum class ComponentTag : unsigned
{
    Unrelated = 0,
    Client = 1,
    Server = 2,
    Plan,
    Action,
    Transport,
    Receiver,
    Dispatcher,
};


enum class LocalPeerTag : unsigned
{
    Client = 1,
    Server = 2,
};

}  // namespace multio::config


namespace eckit {
template <>
struct Translator<multio::config::ComponentTag, std::string> {
    std::string operator()(multio::config::ComponentTag);
};


template <>
struct Translator<multio::config::LocalPeerTag, std::string> {
    std::string operator()(multio::config::LocalPeerTag);
};
}  // namespace eckit


//=============================================================================

namespace multio::config {

struct YAMLFile {
    eckit::LocalConfiguration content;
    eckit::PathName path;
};

struct MPIInitInfo {
    std::optional<int> parentComm{};
    std::optional<std::string> clientId{};
    std::optional<int> defaultClientSplitColor{777};  // Hardcoded defaults may be overwritten
    std::optional<int> defaultServerSplitColor{888};  // Hardcoded defaults may be overwritten
    mutable int* returnClientComm{nullptr};           // Hardcoded defaults may be overwritten
    mutable int* returnServerComm{nullptr};           // Hardcoded defaults may be overwritten
    bool allowWorldAsDefault{true};
};


class MultioConfiguration {
public:
    MultioConfiguration(const eckit::PathName& fileName, LocalPeerTag clientOrServer = LocalPeerTag::Client);
    MultioConfiguration(const eckit::PathName& pathName, const eckit::PathName& fileName,
                  LocalPeerTag clientOrServer = LocalPeerTag::Client);
    MultioConfiguration(const eckit::LocalConfiguration& globalYAMLConfig, const eckit::PathName& pathName,
                  const eckit::PathName& fileName, LocalPeerTag clientOrServer = LocalPeerTag::Client);


    const eckit::LocalConfiguration& YAML() const;
    const eckit::PathName& pathName() const;
    const eckit::PathName& fileName() const;

    MultioConfiguration& setPathName(const eckit::PathName&);

    LocalPeerTag localPeerTag() const;
    bool isServer() const;
    bool isClient() const;
    
    MultioConfiguration& setLocalPeerTag(LocalPeerTag clientOrServer);
    MultioConfiguration& tagServer();
    MultioConfiguration& tagClient();





    const std::optional<MPIInitInfo>& getMPIInitInfo() const;
    std::optional<MPIInitInfo>& getMPIInitInfo();
    MultioConfiguration& setMPIInitInfo(const std::optional<MPIInitInfo>& val);


    const YAMLFile& getYAMLFile(const char*) const;
    const YAMLFile& getYAMLFile(const std::string&) const;

    const YAMLFile& getYAMLFile(const eckit::PathName&) const;

    const YAMLFile& getRelativeYAMLFile(const eckit::PathName&, const char*) const;
    const YAMLFile& getRelativeYAMLFile(const eckit::PathName&, const std::string&) const;

    std::string replaceCurly(const std::string&) const;


    friend config::MetadataMappings;
    const MetadataMappings& metadataMappings() const;

private:
    eckit::LocalConfiguration globalYAML_;
    eckit::PathName pathName_;
    eckit::PathName fileName_;
    LocalPeerTag localPeerTag_;

    std::optional<MPIInitInfo> mpiInitInfo_{MPIInitInfo{}};

    mutable std::unordered_map<std::string, YAMLFile> referencedConfigFiles_;
    mutable std::optional<MetadataMappings> metadataMappings_;
};

}  // namespace multio::config
