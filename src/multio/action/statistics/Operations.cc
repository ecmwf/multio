
#include "Operations.h"

#include <regex>

namespace multio::action {

void parse_file_name( const std::string& file, std::string& opname, std::string& precision ){
    static const std::regex operation_grammar("([a-z]+)_([a-z]+)");
    std::smatch match;
    if (std::regex_match(file, match, operation_grammar)) {
        precision = match[2].str();
        opname    = match[1].str();
    }
    else {
        throw eckit::SeriousBug("Wrong grammar in operation definition : " + file, Here());
    }
};


std::vector<std::unique_ptr<Operation>> make_operations(const std::vector<std::string>& opNames, message::Message msg,
                                                        std::shared_ptr<StatisticsIO>& IOmanager,
                                                        const OperationWindow& win,
                                                        const StatisticsConfiguration& cfg) {

    return multio::util::dispatchPrecisionTag(msg.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        std::vector<std::unique_ptr<Operation>> stats;
        for (const auto& op : opNames) {
            stats.push_back(make_operation<Precision>(op, msg.size(), IOmanager, win, cfg));
            if (cfg.options().solver_send_initial_condition()) {
                stats.back()->init(msg.payload().data(), msg.size(), msg, cfg);
            }
            else {
                if (stats.back()->needStepZero()) {
                    std::ostringstream os;
                    os << "Operation needs step zero and solver does not emit step zero :: " << op << std::endl;
                    throw eckit::UserError(os.str(), Here());
                }
                else {
                    stats.back()->init(msg, cfg);
                }
            }
        }
        return stats;
    });
};



std::vector<std::unique_ptr<Operation>> load_operations(std::shared_ptr<StatisticsIO>& IOmanager,
                                                        const OperationWindow& win,
                                                        const StatisticsOptions& opt) {
    std::vector<std::unique_ptr<Operation>> stats;
    IOmanager->pushDir( "operations" );
    std::vector<eckit::PathName> files = IOmanager->getFiles();
    for (const auto& file : files) {
        std::string opname;
        std::string precision;
        parse_file_name( file.baseName(false).asString(), opname, precision );
        stats.push_back(
            multio::util::dispatchPrecisionTag(precision, [&](auto pt) {
                using Precision = typename decltype(pt)::type;
                return load_operation<Precision>(opname, IOmanager, win,opt );
            }) );
    }
    IOmanager->popDir();
    return stats;
}


}  // namespace multio::action
