/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany
/// @author Tiago Quintino

/// @date Jan 2019

#pragma once

#include <iosfwd>
#include <vector>

#include "StatisticsOptions.h"
#include "multio/action/ChainedAction.h"

namespace eckit {
class Configuration;
}

namespace multio::action {

class TemporalStatistics;

class Statistics : public ChainedAction {
public:
    explicit Statistics(const ComponentConfiguration& compConf);
    ~Statistics();
    void executeImpl(message::Message msg) override;
    message::Metadata outputMetadata(const message::Metadata& inputMetadata, const StatisticsOptions& opt,
                                     const std::string& key, long timeSpanInSeconds) const;

private:
    std::string getKey(const message::Message& msg) const;
    std::string getRestartPartialPath(const message::Message& msg, const StatisticsOptions& opt) const;
    void print(std::ostream& os) const override;
    bool restartExist(const std::string& key, const StatisticsOptions& opt) const;
    const std::string timeUnit_;
    const long timeSpan_;
    const std::vector<std::string> operations_;
    const StatisticsOptions options_;

    std::map<std::string, std::unique_ptr<TemporalStatistics>> fieldStats_;
};

}  // namespace multio::action
