#pragma once

#include <cstdint>
#include <optional>
#include <string>

#include "eckit/config/LocalConfiguration.h"


namespace multio::action::statistics_mtg2 {


enum class WindowType : std::int64_t
{
    ForwardOffset = 0,
    BackwardOffset = 1,
};

/// Where the statistics window starts.
///   WindowStart:   the window is a calendar period of `output-frequency` that is closed and reset after each emission
///   ForecastStart: the window opens at the simulation start and is never reset; `output-frequency` is the cadence at
///                  which the running statistic over the whole forecast so far is emitted
enum class WindowOrigin : std::int64_t
{
    WindowStart = 0,
    ForecastStart = 1,
};

enum class OutputTimeReference : std::int64_t
{
    StartOfForecast = 0,
    StartOfWindow,
};


/*
 * This class handle all the statistics configurations under the option keyword in the yaml
 */
class StatisticsOptions {
private:
    const bool initialConditionPresent_;

    const bool readRestart_;
    const bool writeRestart_;
    const bool debugRestart_;
    const std::string restartTime_;
    const std::string restartPath_;
    const std::string restartPrefix_;
    const std::string restartLib_;

    const std::string logPrefix_;
    const WindowType windowType_;
    const WindowOrigin windowOrigin_;

    const std::optional<std::int64_t> valueCountThreshold_;

    const bool disableStrictMapping_;
    const bool disableSquashing_;
    const bool emitIncompleteStatistics_;
    const bool allowNonUniformStatistics_;
    const bool debug_;
    const std::vector<std::pair<std::string, std::string>> setMetadata_;

    const std::optional<OutputTimeReference> outputTimeReference_;

public:
    StatisticsOptions(const eckit::LocalConfiguration& cfg);

    bool initialConditionPresent() const;

    bool readRestart() const;
    bool writeRestart() const;
    bool debugRestart() const;
    const std::string& restartTime() const;
    const std::string& restartPath() const;
    const std::string& restartPrefix() const;
    const std::string& restartLib() const;

    const std::string& logPrefix() const;
    WindowType windowType() const;
    WindowOrigin windowOrigin() const;

    std::optional<std::int64_t> valueCountThreshold() const;

    bool disableStrictMapping() const;
    bool disableSquashing() const;
    bool emitIncompleteStatistics() const;
    bool allowNonUniformStatistics() const;
    bool debug() const;
    const std::vector<std::pair<std::string, std::string>>& setMetadata() const;

    std::optional<OutputTimeReference> outputTimeReference() const;
};

}  // namespace multio::action::statistics_mtg2
