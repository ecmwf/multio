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
#include "multio/action/statistics/operations/Difference.h"
#include "multio/action/statistics/operations/InverseDifference.h"
#include "multio/action/statistics/operations/FluxAverage.h"
#include "multio/action/statistics/operations/Instant.h"
#include "multio/action/statistics/operations/Maximum.h"
#include "multio/action/statistics/operations/Minimum.h"

#include "multio/action/statistics/operations/DeAccumulate.h"
#include "multio/action/statistics/operations/FixedWindowFluxAverage.h"

namespace multio::action::statistics {

template <typename Precision>
std::unique_ptr<Operation> make_operation(const std::string& opname, std::size_t size, std::shared_ptr<StatisticsIO>& IOmanager,
                                          const OperationWindow& win, const StatisticsConfiguration& cfg) {

    if (opname == "instant") {
        return std::make_unique<Instant<Precision>>(opname, size, win, cfg);
    }
    if (opname == "average") {
        return std::make_unique<Average<Precision>>(opname, size, win, cfg);
    }
    if (opname == "difference") {
        return std::make_unique<Difference<Precision>>(opname, size, win, cfg);
    }
    if (opname == "inverse-difference") {
        return std::make_unique<InverseDifference<Precision>>(opname, size, win, cfg);
    }
    if (opname == "flux-average") {
        return std::make_unique<FluxAverage<Precision>>(opname, size, win, cfg);
    }
    if (opname == "minimum") {
        return std::make_unique<Minimum<Precision>>(opname, size, win, cfg);
    }
    if (opname == "maximum") {
        return std::make_unique<Maximum<Precision>>(opname, size, win, cfg);
    }
    if (opname == "accumulate") {
        return std::make_unique<Accumulate<Precision>>(opname, size, win, cfg);
    }
    if (opname == "de-accumulate") {
        return std::make_unique<DeAccumulate<Precision>>(opname, size, win, cfg);
    }
    if (opname == "fixed-window-flux-average") {
        return std::make_unique<FixedWindowFluxAverage<Precision>>(opname, size, win, cfg);
    }

    std::ostringstream os;
    os << "Invalid opname in statistics operation :: " << opname << std::endl;
    throw eckit::UserError(os.str(), Here());
}


template <typename Precision>
std::unique_ptr<Operation> load_operation(const std::string& opname, std::shared_ptr<StatisticsIO>& IOmanager,
                                          const OperationWindow& win, const StatisticsOptions& opt) {

    if (opname == "instant") {
        return std::make_unique<Instant<Precision>>(opname, win, IOmanager, opt);
    }
    if (opname == "average") {
        return std::make_unique<Average<Precision>>(opname, win, IOmanager, opt);
    }
    if (opname == "difference") {
        return std::make_unique<Difference<Precision>>(opname, win, IOmanager, opt);
    }
    if (opname == "inverse-difference") {
        return std::make_unique<InverseDifference<Precision>>(opname, win, IOmanager, opt);
    }
    if (opname == "flux-average") {
        return std::make_unique<FluxAverage<Precision>>(opname, win, IOmanager, opt);
    }
    if (opname == "minimum") {
        return std::make_unique<Minimum<Precision>>(opname, win, IOmanager, opt);
    }
    if (opname == "maximum") {
        return std::make_unique<Maximum<Precision>>(opname, win, IOmanager, opt);
    }
    if (opname == "accumulate") {
        return std::make_unique<Accumulate<Precision>>(opname, win, IOmanager, opt);
    }
    if (opname == "de-accumulate") {
        return std::make_unique<DeAccumulate<Precision>>(opname, win, IOmanager, opt);
    }
    if (opname == "fixed-window-flux-average") {
        return std::make_unique<FixedWindowFluxAverage<Precision>>(opname, win, IOmanager, opt);
    }

    std::ostringstream os;
    os << "Invalid opname in statistics operation :: " << opname << std::endl;
    throw eckit::UserError(os.str(), Here());
}

std::vector<std::unique_ptr<Operation>> make_operations(const std::vector<std::string>& opNames, message::Message msg,
                                                        std::shared_ptr<StatisticsIO>& IOmanager,
                                                        const OperationWindow& win, const StatisticsConfiguration& cfg);

std::vector<std::unique_ptr<Operation>> load_operations(std::shared_ptr<StatisticsIO>& IOmanager,
                                                        const OperationWindow& win, const StatisticsOptions& opt);

}  // namespace multio::action::statistics
