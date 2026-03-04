#pragma once

#include "StatisticsOptions.h"

#include "eckit/types/DateTime.h"
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
    const std::optional<std::int64_t> timespan_;

    const std::int64_t param_;
    const std::string levType_;
    const std::string gridType_;
    const std::string precision_;

    // Handle missing values
    const std::optional<double> missingValue_;

    // Unique key used for statistics map
    const std::string key_;

    // Timing utils
    const eckit::DateTime epoch_;
    const eckit::DateTime curr_;
    
    const OutputTimeReference outputTimeReference_;

    // ---------------------------------------------------------------------------------------------

    std::string generateKey(const message::Peer& src) const;

    eckit::DateTime computeEpoch() const;
    eckit::DateTime computeCurr() const;


public:
    StatisticsConfiguration(const message::Metadata& md, const message::Peer& src, const StatisticsOptions& opt);
    StatisticsConfiguration(const message::Message& msg, const StatisticsOptions& opt);

    const StatisticsOptions& options() const;

    std::int64_t date() const;
    std::int64_t time() const;
    std::int64_t level() const;
    std::int64_t timeStep() const;
    std::int64_t step() const;
    std::optional<std::int64_t> timespan() const;

    std::int64_t param() const;
    const std::string& levType() const;
    const std::string& gridType() const;
    const std::string& precision() const;

    // Handle missing values
    bool bitmapPresent() const;
    double missingValue() const;

    const std::string& key() const;

    const eckit::DateTime& epoch() const;
    const eckit::DateTime& curr() const;
    
    OutputTimeReference outputTimeReference() const;
};

}  // namespace multio::action::statistics_mtg2
