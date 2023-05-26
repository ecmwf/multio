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

enum class LocalPeerTag : unsigned
{
    Client = 1,
    Server = 2,
};

}  // namespace multio::config


namespace eckit {

template <>
struct Translator<multio::config::LocalPeerTag, std::string> {
    std::string operator()(multio::config::LocalPeerTag);
};

}  // namespace eckit


//=============================================================================

namespace multio::config {

struct ConfigFile {
    eckit::LocalConfiguration content;
    eckit::PathName source;
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
    MultioConfiguration(const eckit::PathName& configFile = configuration_file_name(),
                        LocalPeerTag clientOrServer = LocalPeerTag::Client);
    MultioConfiguration(const eckit::PathName& configDir, const eckit::PathName& configFile,
                        LocalPeerTag clientOrServer = LocalPeerTag::Client);
    MultioConfiguration(const eckit::LocalConfiguration& globalConfigConfig, const eckit::PathName& configDir,
                        const eckit::PathName& configFile, LocalPeerTag clientOrServer = LocalPeerTag::Client);


    eckit::LocalConfiguration& parsedConfig();
    const eckit::LocalConfiguration& parsedConfig() const;
    const eckit::PathName& configDir() const;
    const eckit::PathName& configFile() const;

    void setConfigDir(const eckit::PathName&);

    LocalPeerTag localPeerTag() const;
    bool isServer() const;
    bool isClient() const;

    void setLocalPeerTag(LocalPeerTag clientOrServer);

    const std::optional<MPIInitInfo>& getMPIInitInfo() const;
    std::optional<MPIInitInfo>& getMPIInitInfo();
    void setMPIInitInfo(const std::optional<MPIInitInfo>& val);

    const ConfigFile& getConfigFile(const char*) const;
    const ConfigFile& getConfigFile(const std::string&) const;

    const ConfigFile& getConfigFile(const eckit::PathName&) const;

    const ConfigFile& getRelativeConfigFile(const eckit::PathName&, const char*) const;
    const ConfigFile& getRelativeConfigFile(const eckit::PathName&, const std::string&) const;

    std::string replaceCurly(const std::string&) const;


    const std::vector<message::MetadataMapping>& getMetadataMappings(const std::string& mapping) const;

private:
    eckit::LocalConfiguration parsedConfig_;
    eckit::PathName configDir_;
    eckit::PathName configFile_;
    LocalPeerTag localPeerTag_;

    std::optional<MPIInitInfo> mpiInitInfo_{MPIInitInfo{}};

    mutable std::unordered_map<std::string, ConfigFile> referencedConfigFiles_;
    MetadataMappings metadataMappings_;
};

//=============================================================================


class MultioConfigurationHolder {
protected:
    MultioConfiguration multioConf_;

public:
    MultioConfigurationHolder(MultioConfiguration&& multioConf);
    MultioConfigurationHolder(MultioConfiguration&& multioConf, LocalPeerTag);

    MultioConfiguration& multioConfig() noexcept;
};

//=============================================================================

}  // namespace multio::config
