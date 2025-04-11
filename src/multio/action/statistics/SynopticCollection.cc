
#include "multio/action/statistics/SynopticCollection.h"

#include <iomanip>
#include <iostream>
#include <ostream>
#include <regex>


#include "multio/LibMultio.h"
#include "multio/action/statistics/OperationWindow.h"
#include "multio/action/statistics/Operations.h"


namespace multio::action::statistics {

namespace {

/*
 * Grammar for filter definition in yaml
 *
 * There are two way of define an operation:
 * 1) - <operationName>: in this case the "NoFilter" is assumed,
 *                       hence this configuration is equivalent to
 *                       NoFilter::<operationName>; this way of configuring
 *                       operations is maintained for backward compatibility
 *                       with the old statistics.
 * 2) - <filterName>::<oprationName>: there are two default operations
 *                       always defined that do not need any configuration:
 *                       a) NoFilter: No synoptic filters adopted. All the times are
 *                                    used to update the same operation
 *                       b) DailyHours: All the 24 possible synoptic filters are
 *                                      used
 *                       Otherwise it is possible to define custom operations in which
 *                       it is necessary to specify the field "name", which is used to lookup
 *                       the filter name.
 *
 * There are two kind of filters:
 * 1) - filters that needs special configuration from the action yaml component configuration.
 *      At the moment only the "DailyCustom" filter is present in this category. These filters
 *      are uniquely defined by the pair <filterName,filterType> since it is possible to define
 *      the same filter with two different configurations.
 * 2) - filters that do not need any special configuration. At the moment only the "NoFilter"
 *      that maps all the hours in day in a single operation, and the "DailyHours" the maps every
 *      hour in a different operation. For these two filters, filterName is equal to filterType
 */


// Parse input for requested statistics
const std::array<std::string, 3> parseOperationName(
    const std::string& operation, const std::map<std::string, eckit::LocalConfiguration>& filterConf) {
    static const std::regex op1_grammar("([a-zA-Z0-9]+)::([a-zA-Z]+)");
    static const std::regex op2_grammar("([a-zA-Z]+)");
    std::smatch match1;
    std::smatch match2;
    std::array<std::string, 3> out;
    if (std::regex_match(operation, match1, op1_grammar)) {
        // Handle default filters
        if (match1[1].str() == "NoFilter" || match1[1].str() == "DailyHours") {
            out[0] = match1[1].str();
            out[1] = match1[1].str();
            out[2] = match1[2].str();
            return out;
        }
        // Lookup filter type from filter name
        else if (filterConf.find(match1[1].str()) != filterConf.end()) {
            if (filterConf.at(match1[1].str()).has("type")) {
                out[0] = match1[1].str();
                out[1] = filterConf.at(match1[1].str()).getString("type");
                out[2] = match1[2].str();
                return out;
            }
            else {
                std::ostringstream os;
                os << "Wrong configuration for Operation :: " << std::endl
                   << "current configuration is: \"" << operation << "\""
                   << "valid configuration are : \"<synopticFilterName>::<OperationName>\" or "
                   << "\"<operationName>\"" << std::endl;
                throw eckit::SeriousBug(os.str(), Here());
            }
        }
        else {
            std::ostringstream os;
            os << "Wrong configuration for Operation :: " << std::endl
               << "current configuration is: \"" << operation << "\""
               << "valid configuration are : \"<synopticFilterName>::<OperationName>\" or "
               << "\"<operationName>\"" << std::endl;
            throw eckit::SeriousBug(os.str(), Here());
        }
    }
    // Handle the case in which no filterName is specified
    else if (std::regex_match(operation, match2, op2_grammar)) {
        out[0] = "NoFilter";
        out[1] = "NoFilter";
        out[2] = match2[1].str();
        return out;
    }
    else {
        std::ostringstream os;
        os << "Wrong configuration for Operation :: " << std::endl
           << "current configuration is: \"" << operation << "\""
           << "valid configuration are : \"<synopticFilterName>::<OperationName>\" or "
           << "\"<operationName>\"" << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
}
}  // namespace


// Standard constructor
SynopticCollection::SynopticCollection(const std::string& operation, const message::Message& msg,
                                       std::shared_ptr<StatisticsIO>& IOmanager, const OperationWindow& win,
                                       const std::map<std::string, eckit::LocalConfiguration>& filterConf,
                                       const StatisticsConfiguration& cfg) :
    win_{win},
    op_{parseOperationName(operation, filterConf)},
    filter_{op_[0] == op_[1] ? make_filter(op_[1], cfg) : make_filter(op_[1], filterConf.at(op_[0]), cfg)}  //,
//    statistics_{make_operations(op_[2], msg, filter_->size(), IOmanager, win, cfg)}
{
    LOG_DEBUG_LIB(::multio::LibMultio) << cfg.logPrefix() << " *** SynopticCollection standard constructor "
                                       << std::endl;
};


// Load constructor
SynopticCollection::SynopticCollection(const std::string& operation, const std::string& precision,
                                       std::shared_ptr<StatisticsIO>& IOmanager, const OperationWindow& win,
                                       const std::map<std::string, eckit::LocalConfiguration>& filterConf,
                                       const StatisticsConfiguration& cfg) :
    win_{win},
    op_{parseOperationName(operation, filterConf)},
    filter_{op_[0] == op_[1] ? make_filter(op_[1], cfg) : make_filter(op_[1], filterConf.at(op_[0]), cfg)}  //,
//    statistics_{load_operations(op_[2], precision, filter_->size(), IOmanager, win, cfg)}
{
    LOG_DEBUG_LIB(::multio::LibMultio) << cfg.logPrefix() << " *** SynopticCollection load constructor " << std::endl;
};


// Number of different statistics that are contained in this collection
size_t SynopticCollection::size() const {
    LOG_DEBUG_LIB(::multio::LibMultio) << " *** SynopticCollection::size " << std::endl;
    return filter_->size();
};


void SynopticCollection::resetWindow(const message::Message& msg, const StatisticsConfiguration& cfg) {
    LOG_DEBUG_LIB(::multio::LibMultio) << cfg.logPrefix() << " *** SynopticCollection::updateWindow " << std::endl;
    for (auto& stat : statistics_) {
        // TODO: Just set to zero all the data of the window
        // stat.resetWindow();
    }
    size_t key;
    if (filter_->match(msg, cfg, key)) {
        // TODO: add profiling code
        // TODO: handling the time for a better metadata control
        // TODO: update data must have control over
        statistics_[key]->updateWindow(msg, cfg);
    }
    return;
};


void SynopticCollection::updateData(const message::Message& msg, const StatisticsConfiguration& cfg) {
    LOG_DEBUG_LIB(::multio::LibMultio) << cfg.logPrefix() << " *** SynopticCollection::updateData " << std::endl;
    size_t key;
    if (filter_->match(msg, cfg, key)) {
        // TODO: add profiling code
        // TODO: handling the time for a better metadata control
        statistics_[key]->updateData(msg.Payload(), msg.size(), cfg);
    }
    return;
};


void SynopticCollection::fillMetadata(size_t idx, message::Metadata& metadata) const {
    LOG_DEBUG_LIB(::multio::LibMultio) << " *** SynopticCollection::fillMetadata " << std::endl;
    filter_->fillMetadata(idx, metadata);
    metadata.set("operation", statistics_[idx]->operation());
    return;
};


void SynopticCollection::dump(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsConfiguration& cfg) const {
    LOG_DEBUG_LIB(::multio::LibMultio) << cfg.logPrefix() << " *** SynopticCollection::dump " << std::endl;
    std::ostringstream os;
    size_t cnt = 0;
    for (auto& stat : statistics_) {
        cnt++;
        std::string folder;
        {
            os << std::setw(10) << std::setfill('0') << cnt;
            folder = os.str();
            os.str("");
        }
        // IOmanager->setOperationID(folder);
        // stat->dump(IOmanager, cfg);
    }
    return;
};


const std::unique_ptr<Operation>& SynopticCollection::operator[](size_t idx) {
    if (idx >= statistics_.size()) {
        std::ostringstream os;
        os << "Index out of range :: " << std::endl
           << " - current value is :: " << idx << std::endl
           << " - valid range is   :: [" << 0 << "..." << size() - 1 << "]" << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    LOG_DEBUG_LIB(::multio::LibMultio) << " *** SynopticCollection::operator[" << idx << "]" << std::endl;
    return statistics_[idx];
}


std::string SynopticCollection::filterName() const {
    return std::string{op_[0] + "-" + op_[1] + "-" + op_[2]};
}


std::vector<std::unique_ptr<SynopticCollection>> make_collections(
    const std::vector<std::string>& operations, const message::Message& msg, std::shared_ptr<StatisticsIO>& IOmanager,
    const OperationWindow& win, const std::map<std::string, eckit::LocalConfiguration>& filterConf,
    const StatisticsConfiguration& cfg) {

    std::vector<std::unique_ptr<SynopticCollection>> collections;
    for (const auto& op : operations) {
        // std::string tmp = "DailyHours::" + op;
        // TODO: manage force double precision here
        collections.push_back(std::make_unique<SynopticCollection>(op, msg, IOmanager, win, filterConf, cfg));
    }
    return collections;
}


std::vector<std::unique_ptr<SynopticCollection>> load_collections(
    const std::vector<std::string>& operations, const std::string& precision, std::shared_ptr<StatisticsIO>& IOmanager,
    const OperationWindow& win, const std::map<std::string, eckit::LocalConfiguration>& filterConf,
    const StatisticsConfiguration& cfg) {
    std::array<std::string, 3> fnfton;
    std::vector<std::unique_ptr<SynopticCollection>> collections;
    for (const auto& op : operations) {
        fnfton = parseOperationName(op, filterConf);
        std::string filterName = fnfton[0] + "-" + fnfton[1] + "-" + fnfton[2];
        // IOmanager->setSynopticFilter(filterName);
        collections.push_back(std::make_unique<SynopticCollection>(op, precision, IOmanager, win, filterConf, cfg));
    }
    return collections;
}


}  // namespace multio::action::statistics