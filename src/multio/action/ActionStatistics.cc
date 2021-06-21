
#include "ActionStatistics.h"

namespace multio{
namespace action {

ActionStatistics::ActionStatistics() {}

void ActionStatistics::report(std::ostream& out, const std::string& type,
                              const char* indent) const {
    std::string str = "    -- <" + type + "> timing";
    reportTime(out, str.c_str(), executeTiming_, indent);
}

}  // namespace server
}  // namespace multio
