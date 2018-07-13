
#ifndef multio_server_Plan_H
#define multio_server_Plan_H

#include <memory>
#include <vector>

#include "Action.h"

namespace atlas {
class Field;
}

namespace multio {
namespace server {

class Message;

using ActionList = std::vector<std::unique_ptr<const Action>>;
using ActionListIt = ActionList::const_iterator;

class Plan {
public:
    explicit Plan(std::string nm, ActionList&& actions);

    void process(const Message& msg) const;

private: // members
    const std::string name_;
    ActionList actions_;

private: // methods
    void print(std::ostream&) const;
    friend std::ostream& operator<<(std::ostream& os, const Plan& plan) {
        plan.print(os);
        return os;
    }

    bool moveOntoNextAction(ActionListIt& it, atlas::Field& field) const;
};

}  // namespace server
}  // namespace multio

#endif
