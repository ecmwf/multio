
#pragma once

#include <iosfwd>

#include "multio/util/Timing.h"

namespace multio::action {

class ActionStatistics : public eckit::Statistics {
public:
    ActionStatistics();

    util::Timing<> actionTiming_;

    void report(std::ostream& out, const std::string& type = "Action", const char* indent = "");
};

}  // namespace multio::action
