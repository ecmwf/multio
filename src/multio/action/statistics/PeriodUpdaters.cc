#include "PeriodUpdaters.h"


#include <iomanip>
#include <regex>

#include "TimeUtils.h"

namespace multio::action::statistics {


void parsePeriodGrammar(std::string const& output_freq, long& span, std::string& periodKind) {
    static const std::regex period_grammar("([1-9][0-9]*)([a-z]+)");
    std::smatch match;
    if (std::regex_match(output_freq, match, period_grammar)) {
        periodKind = match[2].str();
        span = std::stol(match[1].str());
    }
    else {
        throw eckit::SeriousBug("Wrong grammar in period definition : " + output_freq, Here());
    }
};


void parsePeriodFileName(std::string const& output_freq, std::string& periodKind) {
    static const std::regex period_grammar("([a-z]+)_([0-9]+)");
    std::smatch match;
    if (std::regex_match(output_freq, match, period_grammar)) {
        periodKind = match[1].str();
    }
    else {
        throw eckit::SeriousBug("Wrong grammar in period definition : " + output_freq, Here());
    }
};


std::unique_ptr<PeriodUpdater> make_period_updater(std::string const& output_freq, const StatisticsConfiguration& cfg) {

    long span;
    std::string periodKind;
    parsePeriodGrammar(output_freq, span, periodKind);

    if (periodKind == "hour" || periodKind == "h") {
        return std::make_unique<HourPeriodUpdater>(span);
    }
    if (periodKind == "day" || periodKind == "d") {
        return std::make_unique<DayPeriodUpdater>(span);
    }
    if (periodKind == "month" || periodKind == "m") {
        return std::make_unique<MonthPeriodUpdater>(span);
    }
    throw eckit::SeriousBug("Unknown period kind : " + periodKind, Here());
};

std::unique_ptr<PeriodUpdater> load_period_updater(std::shared_ptr<StatisticsIO>& IOmanager,
                                                   const StatisticsOptions& opt) {

    std::unique_ptr<PeriodUpdater> ret;
    IOmanager->pushDir("periodUpdater");
    std::vector<eckit::PathName> files = IOmanager->getFiles();

    if (files.empty()) {
        throw eckit::SeriousBug("No files found in: " + IOmanager->getCurrentDir(), Here());
    }
    if (files.size() > 1) {
        throw eckit::SeriousBug("More than one file found in: " + IOmanager->getCurrentDir(), Here());
    }

    std::string periodKind = files[0].baseName(false).asString();
    // parsePeriodFileName( files[0].baseName(false).asString(), periodKind );

    // std::ostringstream logos;
    // logos << "     - Loading periodUpdater from: " << IOmanager->getCurrentDir()  << std::endl;
    // LOG_DEBUG_LIB(LibMultio) << logos.str() << std::endl;

    bool found = false;
    if (periodKind == "hour") {
        found = true;
        ret = std::make_unique<HourPeriodUpdater>(IOmanager, opt);
    }
    if (periodKind == "day") {
        found = true;
        ret = std::make_unique<DayPeriodUpdater>(IOmanager, opt);
    }
    if (periodKind == "month") {
        found = true;
        ret = std::make_unique<MonthPeriodUpdater>(IOmanager, opt);
    }
    IOmanager->popDir();

    if (!found) {
        throw eckit::SeriousBug("Unknown period kind : " + periodKind, Here());
    }

    return ret;
};


}  // namespace multio::action::statistics
