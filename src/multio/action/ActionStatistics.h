
#pragma once

#include <iosfwd>

#include "eckit/log/Statistics.h"

namespace multio {
namespace action {

class ActionStatistics : public eckit::Statistics {
public:
    ActionStatistics();

    eckit::Timing actionTiming_;
    eckit::Timer localTimer_;  // Remove it once eckit::Statistics is fixed

    void report(std::ostream& out, const std::string& type = "Action", const char* indent = "") const;
};

}  // namespace action
}  // namespace multio
