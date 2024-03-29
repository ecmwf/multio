
#include "MpiCommSetup.h"
#include "Transport.h"  // Import TransportException

#include "eckit/thread/AutoLock.h"
#include "eckit/thread/Mutex.h"


namespace multio::transport::mpi {
namespace {

CommSetupType parseType(const std::string& typeString) {
    if (typeString == "passed") {
        return CommSetupType::Passed;
    }
    if (typeString == "split") {
        return CommSetupType::Split;
    }
    return CommSetupType::Unknown;
}

eckit::mpi::Comm* aliasedComm(const std::string& name) {
    if (name == "world") {
        return &eckit::mpi::comm();
    }
    if (eckit::mpi::hasComm(name.c_str())) {
        return &eckit::mpi::comm(name.c_str());
    }
    return nullptr;
}

void addAlias(const std::string& name, const std::string& altName) {
    eckit::mpi::addComm(altName.c_str(), eckit::mpi::comm(name.c_str()).communicator());
}

eckit::mpi::Comm& getCommPreparedCtx(const ComponentConfiguration& compConf, const std::string& name,
                                     const std::optional<CommSetupOptions>& options
                                     = std::optional<CommSetupOptions>{}) {

    auto log = [&compConf, &name](const std::string& msg) {
        eckit::Log::info() << " *** [" << eckit::translate<std::string>(compConf.multioConfig().localPeerTag())
                           << "] mpi::getComm \"" << name << "\" - " << msg << std::endl;
    };

    eckit::mpi::Comm* existingComm = aliasedComm(name);
    if (existingComm != nullptr) {
        return *existingComm;
    }

    const auto& subConfig = compConf.parsedConfig().getSubConfiguration(name);
    std::optional<std::string> typeString = subConfig.has("type")
                                              ? std::optional<std::string>{subConfig.getString("type")}
                                              : std::optional<std::string>{};
    auto commSetupType = typeString
                           ? parseType(*typeString)
                           : ((options && options->defaultType) ? *options->defaultType : CommSetupType::Unknown);


    auto withLog = [&log](eckit::mpi::Comm& comm, const std::string& msg) -> eckit::mpi::Comm& {
        std::ostringstream oss;
        oss << msg << " (Comm: " << comm.communicator() << ", size: " << comm.size() << ", rank: " << comm.rank()
            << ")";
        log(oss.str());
        return comm;
    };

    const auto& mpiInitInfo = compConf.multioConfig().getMPIInitInfo();
    switch (commSetupType) {

        case CommSetupType::Passed: {
            eckit::mpi::Comm& comm = (mpiInitInfo && mpiInitInfo->parentComm)
                                       ? withLog((eckit::mpi::addComm(name.c_str(), *mpiInitInfo->parentComm),
                                                  eckit::mpi::comm(name.c_str())),
                                                 "passed parent comm " + std::to_string(*mpiInitInfo->parentComm))
                                       : [&subConfig, &mpiInitInfo, &withLog, &compConf, &name]() -> eckit::mpi::Comm& {
                bool hasDefault = subConfig.has("default");
                if (!hasDefault && !mpiInitInfo->allowWorldAsDefault) {
                    std::ostringstream oss;
                    oss << "No communicator \"" << name << "\" and no default given.";
                    throw transport::TransportException(oss.str(), Here());
                }
                auto defaultCommName = hasDefault ? subConfig.getString("default") : "world";

                auto& comm = withLog(getCommPreparedCtx(compConf, defaultCommName), "defaults to " + defaultCommName);

                addAlias(defaultCommName, name);
                return comm;
            }();

            return comm;
        }

        case CommSetupType::Split: {
            const auto getSplitColor_ = [&subConfig, &mpiInitInfo, &compConf]() {
                if (subConfig.has("color")) {
                    return std::optional<int>(subConfig.getInt("color"));
                }
                switch (compConf.multioConfig().localPeerTag()) {
                    case config::LocalPeerTag::Client:
                        return mpiInitInfo->defaultClientSplitColor;
                    case config::LocalPeerTag::Server:
                        return mpiInitInfo->defaultServerSplitColor;
                    default:
                        return std::optional<int>{};
                }
            };

            auto splitColor = getSplitColor_();

            if (!splitColor) {
                std::ostringstream oss;
                oss << "No color given in mpi communicator configuration \"" << name
                    << "\". Add a key \"color\" mapping to an integer used for splitting the "
                       "parent communicator.";
                throw transport::TransportException(oss.str(), Here());
            }

            std::optional<std::string> parentName
                = subConfig.has("parent") ? std::optional<std::string>(subConfig.getString("parent"))
                                          : (options ? options->parentCommName : std::optional<std::string>());

            if (!parentName && !mpiInitInfo->allowWorldAsDefault) {
                std::ostringstream oss;
                oss << "No parent communicator name to split \"" << name
                    << "\" from given. Set 'allowWorldAsDefault' to allow using the default world communicator as "
                       "parent.";
                throw transport::TransportException(oss.str(), Here());
            }

            eckit::mpi::Comm& parentComm = getCommPreparedCtx(compConf, parentName ? *parentName : "world");
            std::ostringstream splitLogMsg;
            splitLogMsg << " from " << (parentName ? *parentName : std::string("eckit::mpi default"))
                        << " (Comm: " << parentComm.communicator() << ", size: " << parentComm.size()
                        << ", rank: " << parentComm.rank() << ") by color " << *splitColor;
            log("try splitting" + splitLogMsg.str());


            eckit::mpi::Comm& comm = parentComm.split(*splitColor, name);

            withLog(comm, "successful split " + splitLogMsg.str());

            const auto& mpiInitInfo = compConf.multioConfig().getMPIInitInfo();

            // printComms();
            return comm;
        }

        default: {
            std::ostringstream oss;
            if (typeString) {
                oss << "Unknown type \"" << *typeString << "\"";
            }
            else {
                oss << "No default comm setup type has been passed and no key \"type\" given";
            }
            oss << " in mpi communicator configuration \"" << name << "\"";
            throw transport::TransportException(oss.str(), Here());
        }
    }
}

}  // namespace

eckit::mpi::Comm& getComm(const ComponentConfiguration& compConf, const std::string& name,
                          const std::optional<CommSetupOptions>& options) {
    return getCommPreparedCtx(
        ComponentConfiguration(compConf.multioConfig().parsedConfig().getSubConfiguration("mpi-communicators"),
                               compConf.multioConfig()),
        name, options);
}

}  // namespace multio::transport::mpi
