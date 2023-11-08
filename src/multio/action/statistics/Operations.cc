
#include "Operations.h"

namespace multio::action {

std::vector<std::unique_ptr<Operation>> make_operations(const std::vector<std::string>& opNames, message::Message msg,
                                                        std::shared_ptr<StatisticsIO>& IOmanager,
                                                        const OperationWindow& win,
                                                        const StatisticsConfiguration& cfg) {

    return multio::util::dispatchPrecisionTag(msg.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        std::vector<std::unique_ptr<Operation>> stats;
        for (const auto& op : opNames) {
            stats.push_back(make_operation<Precision>(op, msg.size(), IOmanager, win, cfg));
            if (cfg.solver_send_initial_condition()) {
                stats.back()->init(msg.payload().data(), msg.size());
            }
            else {
                if (stats.back()->needStepZero()) {
                    std::ostringstream os;
                    os << "Operation needs step zero and solver does not emit step zero :: " << op << std::endl;
                    throw eckit::UserError(os.str(), Here());
                }
                else {
                    stats.back()->init();
                }
            }
        }
        return stats;
    });
};

}  // namespace multio::action
