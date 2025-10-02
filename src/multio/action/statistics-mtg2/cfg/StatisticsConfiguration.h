#pragma once

#include "eckit/config/LocalConfiguration.h"
#include "eckit/types/DateTime.h"
#include "multio/action/Action.h"
#include "multio/action/statistics-mtg2/cfg/StatisticsOptions.h"
#include "multio/config/ComponentConfiguration.h"
#include "multio/message/Message.h"

namespace multio::action::statistics_mtg2 {


class StatisticsConfiguration {
private:
    // Options
    const StatisticsOptions& opt_;

    // Metadata to be extracted from the message
    const std::int64_t date_;
    const std::int64_t time_;
    const std::int64_t level_;
    const std::int64_t timeStep_;
    const std::int64_t step_;

    const std::string param_;
    const std::string levType_;
    const std::string gridType_;
    const std::string precision_;

    // Handle missing values
    const std::optional<double> missingValue_;

    // Unique key used for statistics map
    const std::string key_;


    // Timing utils
    mutable eckit::DateTime epoch_;
    mutable eckit::DateTime curr_;
    mutable eckit::DateTime winStart_;

    mutable bool beginningOfHour_;
    mutable bool beginningOfDay_;
    mutable bool beginningOfMonth_;
    mutable bool beginningOfYear_;

    mutable std::function<eckit::DateTime()> computeEpoch_;
    mutable std::function<eckit::DateTime()> computeCurr_;
    mutable std::function<eckit::DateTime()> computeWinStart_;

    mutable std::function<bool()> computeBeginningOfHour_;
    mutable std::function<bool()> computeBeginningOfDay_;
    mutable std::function<bool()> computeBeginningOfMonth_;
    mutable std::function<bool()> computeBeginningOfYear_;


    // ---------------------------------------------------------------------------------------------

    eckit::DateTime computeEpoch() const;
    eckit::DateTime getEpoch() const;
    eckit::DateTime computeCurr() const;
    eckit::DateTime getCurr() const;
    eckit::DateTime computeWinStart() const;
    eckit::DateTime getWinStart() const;

    bool computeBeginningOfHour() const;
    bool isBeginningOfHour() const;
    bool computeBeginningOfDay() const;
    bool isBeginningOfDay() const;
    bool computeBeginningOfMonth() const;
    bool isBeginningOfMonth() const;
    bool computeBeginningOfYear() const;
    bool isBeginningOfYear() const;

    std::string generateKey(const message::Peer& src) const;


public:
    StatisticsConfiguration(const message::Metadata& md, const message::Peer& src, const StatisticsOptions& opt);
    StatisticsConfiguration(const message::Message& msg, const StatisticsOptions& opt);

    const StatisticsOptions& options() const;

    std::int64_t date() const;
    std::int64_t time() const;
    std::int64_t level() const;
    std::int64_t timeStep() const;
    std::int64_t step() const;

    std::string param() const;
    std::string levType() const;
    std::string gridType() const;
    std::string precision() const;

    // Handle missing values
    bool bitmapPresent() const;
    double missingValue() const;

    const std::string& key() const;

    eckit::DateTime epoch() const;
    eckit::DateTime curr() const;
    eckit::DateTime winStart() const;

    bool beginningOfHour() const;
    bool beginningOfDay() const;
    bool beginningOfMonth() const;
    bool beginningOfYear() const;
};

}  // namespace multio::action::statistics_mtg2
