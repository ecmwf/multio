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


#include "PeriodUpdaters.h"
#include "StatisticsIO.h"
#include "RemapParamID.h"
#include "multio/action/ChainedAction.h"
#include "multio/action/statistics/cfg/StatisticsOptions.h"

namespace eckit {
class Configuration;
}

namespace multio::action {

class TemporalStatistics;

class Statistics : public ChainedAction {
public:
    explicit Statistics(const ComponentConfiguration& compConf);
    void executeImpl(message::Message msg) override;
    message::Metadata outputMetadata(const message::Metadata& inputMetadata, const StatisticsConfiguration& opt,
                                     const std::string& key) const;

private:
    bool needRestart_;
    std::string lastDateTime_;
    void TryDumpRestart(const message::Message& msg);
    std::string generateRestartNameFromFlush(const message::Message& msg) const;
    void DeleteLatestSymLink();
    void CreateLatestSymLink();
    void CreateMainRestartDirectory( const std::string& restartFolderName, bool is_master );
    void DumpTemporalStatistics();
    std::unique_ptr<TemporalStatistics> LoadTemporalStatisticsFromKey(const std::string& key);
    bool HasRestartKey(const std::string& key);
    bool HasMainRestartDir();
    void updateLatestDateTime( const StatisticsConfiguration& cfg );
    void print(std::ostream& os) const override;
    const StatisticsOptions opt_;
    const std::vector<std::string> operations_;
    std::string outputFrequency_;
    RemapParamID remapParamID_;
    std::shared_ptr<StatisticsIO> IOmanager_;

    std::map<std::string, std::unique_ptr<TemporalStatistics>> fieldStats_;
};

}  // namespace multio::action
