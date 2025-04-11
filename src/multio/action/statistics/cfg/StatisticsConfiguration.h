#pragma once

#include "eckit/config/LocalConfiguration.h"
#include "eckit/types/DateTime.h"
#include "multio/action/Action.h"
#include "multio/action/statistics/cfg/StatisticsOptions.h"
#include "multio/config/ComponentConfiguration.h"
#include "multio/message/Message.h"

namespace multio::action::statistics {


class StatisticsConfiguration {
private:
    // Options
    const StatisticsOptions& opt_;

    // Metadata to be extracted from the message
    long date_;
    long time_;
    long level_;
    long timeStep_;
    long stepFreq_;
    long step_;

    std::string param_;
    std::string levType_;
    std::string gridType_;
    std::string precision_;
    std::string logPrefix_;

    // Handle missing values
    bool bitmapPresent_;
    double missingValue_;

    // Unique key used for statistics map
    std::string key_;


    // Timing utils
    mutable eckit::DateTime epoch_;
    mutable eckit::DateTime prev_;
    mutable eckit::DateTime curr_;
    mutable eckit::DateTime next_;
    mutable eckit::DateTime winStart_;

    mutable bool beginningOfHour_;
    mutable bool beginningOfDay_;
    mutable bool beginningOfMonth_;
    mutable bool beginningOfYear_;

    mutable std::function<eckit::DateTime()> computeEpoch_;
    mutable std::function<eckit::DateTime()> computePrev_;
    mutable std::function<eckit::DateTime()> computeCurr_;
    mutable std::function<eckit::DateTime()> computeNext_;
    mutable std::function<eckit::DateTime()> computeWinStart_;

    mutable std::function<bool()> computeBeginningOfHour_;
    mutable std::function<bool()> computeBeginningOfDay_;
    mutable std::function<bool()> computeBeginningOfMonth_;
    mutable std::function<bool()> computeBeginningOfYear_;


    // ---------------------------------------------------------------------------------------------

    eckit::DateTime computeEpoch() const;
    eckit::DateTime getEpoch() const;
    eckit::DateTime computePrev() const;
    eckit::DateTime getPrev() const;
    eckit::DateTime computeCurr() const;
    eckit::DateTime getCurr() const;
    eckit::DateTime computeNext() const;
    eckit::DateTime getNext() const;
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

    void generateKey(const message::Metadata& md, const std::string& src);

    void readPrecision(const message::Metadata& md, const StatisticsOptions& opt);
    void readGridType(const message::Metadata& md, const StatisticsOptions& opt);
    void readLevType(const message::Metadata& md, const StatisticsOptions& opt);
    void readLevel(const message::Metadata& md, const StatisticsOptions& opt);
    void readParam(const message::Metadata& md, const StatisticsOptions& opt);
    void readStartTime(const message::Metadata& md, const StatisticsOptions& opt);
    void readStartDate(const message::Metadata& md, const StatisticsOptions& opt);
    void readStep(const message::Metadata& md, const StatisticsOptions& opt);
    void readTimeStep(const message::Metadata& md, const StatisticsOptions& opt);
    void readStepFrequency(const message::Metadata& md, const StatisticsOptions& opt);
    void readMissingValue(const message::Metadata& md, const StatisticsOptions& opt);


public:
    StatisticsConfiguration(const message::Message& msg, const StatisticsOptions& opt);

    const StatisticsOptions& options() const;
    const std::string& key() const;

    long date() const;
    long time() const;
    long level() const;
    long timeStep() const;
    long stepFreq() const;
    long step() const;

    std::string param() const;
    std::string levType() const;
    std::string gridType() const;
    std::string precision() const;
    std::string logPrefix() const;

    // Handle missing values
    bool bitmapPresent() const;
    double missingValue() const;


    eckit::DateTime epoch() const;
    eckit::DateTime prev() const;
    eckit::DateTime curr() const;
    eckit::DateTime next() const;
    eckit::DateTime winStart() const;

    bool beginningOfHour() const;
    bool beginningOfDay() const;
    bool beginningOfMonth() const;
    bool beginningOfYear() const;
};

}  // namespace multio::action::statistics