
#include "MpiCommSetup.h"
#include "Transport.h"  // Import TransportException

#include "eckit/thread/AutoLock.h"
#include "eckit/thread/Mutex.h"


namespace multio {
namespace transport {
namespace mpi {

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

eckit::mpi::Comm& getCommPreparedCtx(const ConfigurationContext& confCtx, const std::string& name,
                                     const eckit::Optional<CommSetupOptions>& options
                                     = eckit::Optional<CommSetupOptions>{}) {

    auto log = [&confCtx, &name](const std::string& msg) {
        eckit::Log::info() << " *** [" << eckit::translate<std::string>(confCtx.localPeerTag()) << "] mpi::getComm \""
                           << name << "\" - " << msg << std::endl;
    };

    eckit::mpi::Comm* existingComm = aliasedComm(name);
    if (existingComm != nullptr) {
        if (options && options().alias && !eckit::mpi::hasComm(options().alias().c_str())) {
            log("alias: " + options().alias());
            addAlias(name, options().alias());
        }
        return *existingComm;
    }
    else if (options && options().alias && eckit::mpi::hasComm(options().alias().c_str())) {
        existingComm = aliasedComm(options().alias());
        if (existingComm != nullptr) {
            log("alias already exists - reverse map: " + options().alias());
            addAlias(options().alias(), name);
            return *existingComm;
        }
    }
    const auto& subConfig = confCtx.config().getSubConfiguration(name);
    eckit::Optional<std::string> typeString = subConfig.has("type")
                                                ? eckit::Optional<std::string>{subConfig.getString("type")}
                                                : eckit::Optional<std::string>{};
    auto commSetupType = typeString
                           ? parseType(typeString())
                           : ((options && options().defaultType) ? options().defaultType() : CommSetupType::Unknown);


    auto withLog = [&log](eckit::mpi::Comm& comm, const std::string& msg) -> eckit::mpi::Comm& {
        std::ostringstream oss;
        oss << msg << " (Comm: " << comm.communicator() << ", size: " << comm.size() << ", rank: " << comm.rank()
            << ")";
        log(oss.str());
        return comm;
    };

    const auto& mpiInitInfo = confCtx.getMPIInitInfo();
    switch (commSetupType) {

        case CommSetupType::Passed: {
            eckit::mpi::Comm& comm = (mpiInitInfo && mpiInitInfo().parentComm)
                                       ? withLog((eckit::mpi::addComm(name.c_str(), mpiInitInfo().parentComm()),
                                                  eckit::mpi::comm(name.c_str())),
                                                 "passed parent comm " + std::to_string(mpiInitInfo().parentComm()))
                                       : [&subConfig, &mpiInitInfo, &withLog, &confCtx, &name]() -> eckit::mpi::Comm& {
                bool hasDefault = subConfig.has("default");
                if (!hasDefault && !mpiInitInfo().allowWorldAsDefault) {
                    std::ostringstream oss;
                    oss << "No communicator \"" << name << "\" and no default given.";
                    throw transport::TransportException(oss.str(), Here());
                }
                auto defaultCommName = hasDefault ? subConfig.getString("default") : "world";

                auto& comm = withLog(getCommPreparedCtx(confCtx, defaultCommName), "defaults to " + defaultCommName);

                addAlias(defaultCommName, name);
                return comm;
            }();

            if (options && options().alias && !eckit::mpi::hasComm(options().alias().c_str())) {
                log("alias: " + options().alias());
                addAlias(name, options().alias());
            }

            return comm;
        }

        case CommSetupType::Split: {
            const auto getSplitColor_ = [&subConfig, &mpiInitInfo, &confCtx]() {
                if (subConfig.has("color")) {
                    return eckit::Optional<int>(subConfig.getInt("color"));
                }
                switch (confCtx.localPeerTag()) {
                    case util::LocalPeerTag::Client:
                        return mpiInitInfo().defaultClientSplitColor;
                    case util::LocalPeerTag::Server:
                        return mpiInitInfo().defaultServerSplitColor;
                    default:
                        return eckit::Optional<int>{};
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

            eckit::Optional<std::string> parentName
                = subConfig.has("parent") ? eckit::Optional<std::string>(subConfig.getString("parent"))
                                          : (options ? options().parentCommName : eckit::Optional<std::string>());

            if (!parentName && !mpiInitInfo().allowWorldAsDefault) {
                std::ostringstream oss;
                oss << "No parent communicator name to split \"" << name
                    << "\" from given. Set 'allowWorldAsDefault' to allow using the default world communicator as "
                       "parent.";
                throw transport::TransportException(oss.str(), Here());
            }

            eckit::mpi::Comm& parentComm = getCommPreparedCtx(confCtx, parentName ? parentName() : "world");
            std::ostringstream splitLogMsg;
            splitLogMsg << " from " << (parentName ? parentName() : std::string("eckit::mpi default"))
                        << " (Comm: " << parentComm.communicator() << ", size: " << parentComm.size()
                        << ", rank: " << parentComm.rank() << ") by color " << splitColor();
            log("try splitting" + splitLogMsg.str());


            eckit::mpi::Comm& comm = parentComm.split(splitColor(), name);

            withLog(comm, "successful split " + splitLogMsg.str());

            const auto& mpiInitInfo = confCtx.getMPIInitInfo();
            if (options && options().alias && !eckit::mpi::hasComm(options().alias().c_str())) {
                log("alias: " + options().alias());
                addAlias(name, options().alias());
            }
            // printComms();
            return comm;
        }

        default: {
            std::ostringstream oss;
            if (typeString) {
                oss << "Unknown type \"" << typeString() << "\"";
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

eckit::mpi::Comm& getComm(const ConfigurationContext& confCtx, const std::string& name,
                          const eckit::Optional<CommSetupOptions>& options) {
    return getCommPreparedCtx(confCtx.recast(confCtx.globalConfig().getSubConfiguration("mpi-communicators")), name,
                              options);
}

}  // namespace mpi
}  // namespace transport
}  // namespace multio
