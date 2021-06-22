
#ifndef multio_action_ActionStatistics_H
#define multio_action_ActionStatistics_H

#include <iosfwd>

#include <eckit/log/Statistics.h>

namespace multio {
namespace action {

class ActionStatistics : public eckit::Statistics {
public:
    ActionStatistics();

    eckit::Timing actionTiming_;

    void report(std::ostream& out, const std::string& type = "Action",
                const char* indent = "") const;
};

}  // namespace server
}  // namespace multio


#endif // multio_server_ActionStatistics_H
