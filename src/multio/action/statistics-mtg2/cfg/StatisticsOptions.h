#pragma once

#include <cstdint>
#include <string>

#include "eckit/config/LocalConfiguration.h"
#include "multio/action/Action.h"
#include "multio/config/ComponentConfiguration.h"
#include "multio/message/Message.h"


namespace multio::action::statistics_mtg2 {

/*
 * This class handle all the statistics configurations under the option keyword in the yaml
 */
class StatisticsOptions {
private:
    const std::int64_t timeStep_;
    const bool initialConditionPresent_;

    const bool readRestart_;
    const bool writeRestart_;
    const bool debugRestart_;
    const std::string restartTime_;
    const std::string restartPath_;
    const std::string restartPrefix_;
    const std::string restartLib_;

    const std::string logPrefix_;
    const std::string windowType_;
    const std::string solverResetAccumulatedFieldsEvery_;

    const std::optional<long> valueCountThreshold_;

    const bool disableStrictMapping_;
    const bool disableSquashing_;
    const std::vector<std::pair<std::string, std::string>> setMetadata_;

public:
    StatisticsOptions(const eckit::LocalConfiguration& cfg);

    long timeStep() const;
    bool initialConditionPresent() const;

    bool readRestart() const;
    bool writeRestart() const;
    bool debugRestart() const;
    const std::string& restartTime() const;
    const std::string& restartPath() const;
    const std::string& restartPrefix() const;
    const std::string& restartLib() const;

    const std::string& logPrefix() const;
    const std::string& windowType() const;
    const std::string& solverResetAccumulatedFieldsEvery() const;

    std::optional<std::int64_t> valueCountThreshold() const;

    bool disableStrictMapping() const;
    bool disableSquashing() const;
    const std::vector<std::pair<std::string, std::string>>& setMetadata() const;
};

}  // namespace multio::action::statistics_mtg2
