#pragma once

#include <cmath>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include "eckit/exception/Exceptions.h"

#include "multio/LibMultio.h"
#include "multio/message/Message.h"

#include "multio/action/statistics/cfg/StatisticsConfiguration.h"

#include "multio/action/statistics/operations/Operation.h"
#include "multio/action/statistics/operations/OperationWithData.h"
#include "multio/action/statistics/operations/OperationWithDeaccumulatedData.h"

#include "multio/action/statistics/operations/Accumulate.h"
#include "multio/action/statistics/operations/Average.h"
#include "multio/action/statistics/operations/FluxAverage.h"
#include "multio/action/statistics/operations/Instant.h"
#include "multio/action/statistics/operations/Maximum.h"
#include "multio/action/statistics/operations/Minimum.h"

#include "multio/action/statistics/operations/DeAccumulate.h"
#include "multio/action/statistics/operations/FixedWindowFluxAverage.h"

namespace multio::action {

template <typename Precision>
std::unique_ptr<Operation> make_operation(const std::string& opname, long sz, std::shared_ptr<StatisticsIO>& IOmanager,
                                          const OperationWindow& win, const StatisticsConfiguration& cfg) {

    if (opname == "instant") {
        return std::make_unique<Instant<Precision>>(opname, sz, win, cfg);
    }
    if (opname == "average") {
        return std::make_unique<Average<Precision>>(opname, sz, win, cfg);
    }
    if (opname == "flux-average") {
        return std::make_unique<FluxAverage<Precision>>(opname, sz, win, cfg);
    }
    if (opname == "minimum") {
        return std::make_unique<Minimum<Precision>>(opname, sz, win, cfg);
    }
    if (opname == "maximum") {
        return std::make_unique<Maximum<Precision>>(opname, sz, win, cfg);
    }
    if (opname == "accumulate") {
        return std::make_unique<Accumulate<Precision>>(opname, sz, win, cfg);
    }
    if (opname == "de-accumulate") {
        return std::make_unique<DeAccumulate<Precision>>(opname, sz, win, cfg);
    }
    if (opname == "fixed-window-flux-average") {
        return std::make_unique<FixedWindowFluxAverage<Precision>>(opname, sz, win, cfg);
    }

    std::ostringstream os;
    os << "Invalid opname in statistics operation :: " << opname << std::endl;
    throw eckit::UserError(os.str(), Here());
}


template <typename Precision>
std::unique_ptr<Operation> load_operation( const std::string& opname, std::shared_ptr<StatisticsIO>& IOmanager,
                                           const OperationWindow& win, const StatisticsOptions& opt) {

    std::unique_ptr<Operation> ret;
    bool found = false;

    if (opname == "instant") {
        found = true;
        ret = std::make_unique<Instant<Precision>>(opname, win, IOmanager, opt);
    }
    if (opname == "average") {
        found = true;
        ret = std::make_unique<Average<Precision>>(opname, win, IOmanager, opt);
    }
    if (opname == "flux-average") {
        found = true;
        ret = std::make_unique<FluxAverage<Precision>>(opname, win, IOmanager, opt);
    }
    if (opname == "minimum") {
        found = true;
        ret = std::make_unique<Minimum<Precision>>(opname, win, IOmanager, opt);
    }
    if (opname == "maximum") {
        found = true;
        ret = std::make_unique<Maximum<Precision>>(opname, win, IOmanager, opt);
    }
    if (opname == "accumulate") {
        found = true;
        ret = std::make_unique<Accumulate<Precision>>(opname, win, IOmanager, opt);
    }
    if (opname == "de-accumulate") {
        found = true;
        ret = std::make_unique<DeAccumulate<Precision>>(opname, win, IOmanager, opt);
    }
    if (opname == "fixed-window-flux-average") {
        found = true;
        ret = std::make_unique<FixedWindowFluxAverage<Precision>>(opname, win, IOmanager, opt);
    }

    if ( !found ){
      std::ostringstream os;
      os << "Invalid opname in statistics operation :: " << opname << std::endl;
      throw eckit::UserError(os.str(), Here());
    }

    return ret;

}

std::vector<std::unique_ptr<Operation>> make_operations(const std::vector<std::string>& opNames, message::Message msg,
                                                        std::shared_ptr<StatisticsIO>& IOmanager,
                                                        const OperationWindow& win, const StatisticsConfiguration& cfg);

std::vector<std::unique_ptr<Operation>> load_operations(std::shared_ptr<StatisticsIO>& IOmanager,
                                                        const OperationWindow& win, const StatisticsOptions& opt);

}  // namespace multio::action
