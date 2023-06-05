#pragma once

#include <cmath>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include "StatisticsConfiguration.h"
#include "StatisticsIO.h"
#include "multio/LibMultio.h"
#include "multio/message/Message.h"

#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/io/Buffer.h"
#include "eckit/log/Log.h"
#include "eckit/types/DateTime.h"

#include "MovingWindow.h"

#include "operations/OperationBase.h"
#include "operations/OperationWithData.h"

#include "operations/Accumulate.h"
#include "operations/Average.h"
#include "operations/FluxAverage.h"
#include "operations/Instant.h"
#include "operations/Maximum.h"
#include "operations/Minimum.h"

namespace multio::action {


template <typename Precision>
std::unique_ptr<OperationBase> make_operation(const std::string& opname, long sz,
                                              std::shared_ptr<StatisticsIO>& IOmanager, const MovingWindow& win,
                                              const StatisticsConfiguration& cfg) {

    if (opname == "instant") {
        return cfg.readRestart() ? std::make_unique<Instant<Precision>>(opname, sz, win, IOmanager, cfg)
                                 : std::make_unique<Instant<Precision>>(opname, sz, win, cfg);
    }
    if (opname == "average") {
        return cfg.readRestart() ? std::make_unique<Average<Precision>>(opname, sz, win, IOmanager, cfg)
                                 : std::make_unique<Average<Precision>>(opname, sz, win, cfg);
    }
    if (opname == "flux-average") {
        return cfg.readRestart() ? std::make_unique<FluxAverage<Precision>>(opname, sz, win, IOmanager, cfg)
                                 : std::make_unique<FluxAverage<Precision>>(opname, sz, win, cfg);
    }
    if (opname == "minimum") {
        return cfg.readRestart() ? std::make_unique<Minimum<Precision>>(opname, sz, win, IOmanager, cfg)
                                 : std::make_unique<Minimum<Precision>>(opname, sz, win, cfg);
    }
    if (opname == "maximum") {
        return cfg.readRestart() ? std::make_unique<Maximum<Precision>>(opname, sz, win, IOmanager, cfg)
                                 : std::make_unique<Maximum<Precision>>(opname, sz, win, cfg);
    }
    if (opname != "accumulate") {
        return cfg.readRestart() ? std::make_unique<Accumulate<Precision>>(opname, sz, win, IOmanager, cfg)
                                 : std::make_unique<Accumulate<Precision>>(opname, sz, win, cfg);
    }

    std::ostringstream os;
    os << "Invalid opname in statistics operation :: " << opname << std::endl;
    throw eckit::UserError(os.str(), Here());
}

std::vector<std::unique_ptr<OperationBase>> make_operations(const std::vector<std::string>& opNames,
                                                            message::Message msg,
                                                            std::shared_ptr<StatisticsIO>& IOmanager,
                                                            const MovingWindow& win,
                                                            const StatisticsConfiguration& cfg);

}  // namespace multio::action
