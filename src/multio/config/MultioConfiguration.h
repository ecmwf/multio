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
#include <memory>
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

//=============================================================================

struct CFailureInteroperator;
typedef void (*FailureHandlerPtr)(void*, int err_code, CFailureInteroperator*);

struct CFailureInteroperator {
    FailureHandlerPtr handler{nullptr};
    void* context{nullptr};
    // Put the error string in a shared ptr to preserve the location when moving Ownership of the configuration to the
    // handle
    std::string lastErrorString{};
};

//=============================================================================


struct ConfigFile {
    eckit::LocalConfiguration content;
    eckit::PathName source;
};

//=============================================================================

struct MPIInitInfo {
    std::optional<int> parentComm{};
    std::optional<int> defaultClientSplitColor{777};  // Hardcoded defaults may be overwritten
    std::optional<int> defaultServerSplitColor{888};  // Hardcoded defaults may be overwritten
    mutable int* returnClientComm{nullptr};           // Hardcoded defaults may be overwritten
    mutable int* returnServerComm{nullptr};           // Hardcoded defaults may be overwritten
    bool allowWorldAsDefault{true};
};

//=============================================================================


class MultioConfiguration {
public:
    MultioConfiguration(const eckit::PathName& configFile = configuration_file_name(),
                        LocalPeerTag clientOrServer = LocalPeerTag::Client);

    MultioConfiguration(const eckit::LocalConfiguration& globalConfig,
                        LocalPeerTag clientOrServer = LocalPeerTag::Client);

    eckit::LocalConfiguration& parsedConfig();
    const eckit::LocalConfiguration& parsedConfig() const;
    const eckit::PathName& configDir() const;
    const eckit::PathName& configFile() const;

    void setConfigDir(const eckit::PathName&);

    LocalPeerTag localPeerTag() const;

    void setLocalPeerTag(LocalPeerTag clientOrServer);

    const std::optional<MPIInitInfo>& getMPIInitInfo() const;
    std::optional<MPIInitInfo>& getMPIInitInfo();
    void setMPIInitInfo(const std::optional<MPIInitInfo>& val);

    const ConfigFile& getConfigFile(const std::string&) const;

    const ConfigFile& getConfigFile(const eckit::PathName&) const;

    std::string replaceCurly(const std::string&) const;

    const std::vector<message::MetadataMapping>& getMetadataMappings(const std::string& mapping) const;

    
    
    MultioConfiguration(const MultioConfiguration& other) = delete;
    MultioConfiguration& operator=(const MultioConfiguration& other) = delete;
    
    // Custum move assigment & construction to avoid
    // stealing of shared_ptr if use_count = 1 - otherwise this would
    // make weak_ptr to lose track. This can happen when moving ownership to MultioServer/Client.
    MultioConfiguration(MultioConfiguration&& other) noexcept: 
        parsedConfig_(std::move(other.parsedConfig_)), 
        configDir_(std::move(other.configDir_)), 
        configFile_(std::move(other.configFile_)), 
        localPeerTag_(std::move(other.localPeerTag_)), 
        mpiInitInfo_(std::move(other.mpiInitInfo_)), 
        referencedConfigFiles_(std::move(other.referencedConfigFiles_)), 
        metadataMappings_(std::move(other.metadataMappings_)), 
        cFailureInteroperator_(other.cFailureInteroperator_) 
        {
    }
    
    MultioConfiguration& operator=(MultioConfiguration&& other) noexcept {
        parsedConfig_ = std::move(other.parsedConfig_); 
        configDir_ = std::move(other.configDir_); 
        configFile_ = std::move(other.configFile_); 
        localPeerTag_ = std::move(other.localPeerTag_); 
        mpiInitInfo_ = std::move(other.mpiInitInfo_); 
        referencedConfigFiles_ = std::move(other.referencedConfigFiles_); 
        metadataMappings_ = std::move(other.metadataMappings_); 
        cFailureInteroperator_ = other.cFailureInteroperator_; 
        return *this;
    }
    
    // We are using a weak_ptr to pass around the failure handler in the API.
    std::weak_ptr<CFailureInteroperator> getCFailureInteroperator() const;
    

private:
    MultioConfiguration(const eckit::PathName& configDir, const eckit::PathName& configFile,
                        LocalPeerTag clientOrServer = LocalPeerTag::Client);
    MultioConfiguration(const eckit::LocalConfiguration& globalConfig, const eckit::PathName& configDir,
                        const eckit::PathName& configFile, LocalPeerTag clientOrServer = LocalPeerTag::Client);

    eckit::LocalConfiguration parsedConfig_;
    eckit::PathName configDir_;
    eckit::PathName configFile_;
    LocalPeerTag localPeerTag_;

    std::optional<MPIInitInfo> mpiInitInfo_{MPIInitInfo{}};

    mutable std::unordered_map<std::string, ConfigFile> referencedConfigFiles_;
    MetadataMappings metadataMappings_;

    // Use a shared_ptr to provide a location for the failure handler that can outlive errors happening 
    // when creating a handle and moving ownership of the MultioConfiguration.
    // In this case the initial configuration and the new handle keep track of the object until the
    // client/server is fully created and the user has released the multio_configuration object through the API.
    //
    // With a unique_ptr & direct ownership movement, errors while creating the handle (e.g. configuration or MPI errors)
    // could not be handled because the destructor would get called before the handler is used.
    std::shared_ptr<CFailureInteroperator> cFailureInteroperator_;
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
