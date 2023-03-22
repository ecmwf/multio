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

namespace multio {
namespace action {

class TemporalStatistics;

class Statistics : public ChainedAction {
public:
    explicit Statistics(const ConfigurationContext& confCtx);

    void executeImpl(message::Message msg) override;

private:
    void print(std::ostream& os) const override;
    const std::string timeUnit_;
    const long timeSpan_;
    const std::vector<std::string> operations_;
    const StatisticsOptions options_;

    std::map<std::string, std::unique_ptr<TemporalStatistics>> fieldStats_;
};

}  // namespace action
}  // namespace multio
