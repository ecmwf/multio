#pragma once


#include <cstdint>
#include <memory>
#include <string>
#include <vector>

#include "multio/action/ChainedAction.h"
#include "multio/message/Message.h"


#include "multio/action/statistics/OperationWindow.h"
#include "multio/action/statistics/StatisticsIO.h"
#include "multio/action/statistics/SynopticFilters.h"
#include "multio/action/statistics/cfg/StatisticsConfiguration.h"
#include "multio/action/statistics/operations/Operation.h"
#include "multio/config/ComponentConfiguration.h"


namespace multio::action {
// Single operation (i.e. average, stddev) but all the possible "flavours"
// A "flavour" is a subset of the available samples
//
// i.e. [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]
//
class SynopticCollection {
private:
    // Reference to the parent object
    const OperationWindow& win_;

    // Kind of operation (filter_name, filter_type, operation_name)
    const std::array<std::string, 3> op_;

    // Pointer to the matcher used to construct the operation-flavours
    const std::unique_ptr<SynopticFilter> filter_;


    // memory space used to save the data of partial operations
    std::vector<std::unique_ptr<Operation>> statistics_;

public:
    // Standard constructor for synoptic collection
    SynopticCollection(const std::string& operation, const message::Message& msg,
                       std::shared_ptr<StatisticsIO>& IOmanager, const OperationWindow& win,
                       const std::map<std::string, eckit::LocalConfiguration>& filterConf,
                       const StatisticsConfiguration& cfg);

    // Load constructor for synoptic collection
    SynopticCollection(const std::string& operation, const std::string& precision,
                       std::shared_ptr<StatisticsIO>& IOmanager, const OperationWindow& win,
                       const std::map<std::string, eckit::LocalConfiguration>& filterConf,
                       const StatisticsConfiguration& cfg);


    const std::unique_ptr<Operation>& operator[](size_t idx);

    size_t size() const;

    std::string filterName() const;
    void fillMetadata(size_t idx, message::Metadata& metadata) const;
    void dump(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsConfiguration& cfg) const;
    void resetWindow(const message::Message& msg, const StatisticsConfiguration& cfg);
    void updateData(const message::Message& msg, const StatisticsConfiguration& cfg);
};


std::vector<std::unique_ptr<SynopticCollection>> make_collections(
    const std::vector<std::string>& operations, const message::Message& msg, std::shared_ptr<StatisticsIO>& IOmanager,
    const OperationWindow& win, const std::map<std::string, eckit::LocalConfiguration>& filterConf,
    const StatisticsConfiguration& cfg);


std::vector<std::unique_ptr<SynopticCollection>> load_collections(
    const std::vector<std::string>& operations, const std::string& precision, std::shared_ptr<StatisticsIO>& IOmanager,
    const OperationWindow& win, const std::map<std::string, eckit::LocalConfiguration>& filterConf,
    const StatisticsConfiguration& cfg);

}  // namespace multio::action