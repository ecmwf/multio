
#pragma once

#include <string>

#include "multio/action/statistics/OperationWindow.h"
#include "multio/action/statistics/StatisticsConfiguration.h"
#include "multio/action/statistics/StatisticsIO.h"

namespace multio::action {

class Operation {
public:
    Operation(const std::string& name, const std::string& operation, const OperationWindow& win,
              const StatisticsConfiguration& cfg) :
        name_{name}, operation_{operation}, logHeader_{"operation(" + name_ + ")"}, cfg_{cfg}, win_{win} {};

    virtual ~Operation() = default;

    const std::string& name() const { return name_; };
    const std::string& operation() const { return operation_; };

    virtual void updateData(const void* val, long sz) = 0;
    virtual void updateWindow(const void* data, long sz) = 0;

    virtual void dump(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsConfiguration& cfg) const = 0;
    virtual void load(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsConfiguration& cfg) = 0;

    virtual size_t byte_size() const = 0;
    virtual void compute(eckit::Buffer& buf) = 0;
    virtual void init(const void* data, long sz) = 0;
    virtual void init() = 0;

protected:
    virtual void print(std::ostream& os) const = 0;

    const std::string name_;
    const std::string operation_;
    const std::string logHeader_;
    const StatisticsConfiguration& cfg_;
    const OperationWindow& win_;

    friend std::ostream& operator<<(std::ostream& os, const Operation& a) {
        a.print(os);
        return os;
    }
};

}  // namespace multio::action
