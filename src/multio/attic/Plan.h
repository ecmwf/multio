
#ifndef multio_attic_Plan_H
#define multio_attic_Plan_H

#include <memory>
#include <vector>

#include "Action.h"

namespace multio {
namespace attic {

class Message;

class Plan {
public:
    explicit Plan(const std::string& nm, std::unique_ptr<Action>&& root);

    void process(std::shared_ptr<Message> msg) const;

private:  // members
    const std::string name_;
    std::unique_ptr<Action> root_;

private:  // methods
    void print(std::ostream&) const;
    friend std::ostream& operator<<(std::ostream& os, const Plan& plan) {
        plan.print(os);
        return os;
    }

    friend bool operator==(const Plan& lhs, const Plan& rhs);
    friend bool operator!=(const Plan& lhs, const Plan& rhs);
    friend bool operator<(const Plan& lhs, const Plan& rhs);
    friend bool operator<=(const Plan& lhs, const Plan& rhs);
    friend bool operator>(const Plan& lhs, const Plan& rhs);
    friend bool operator>=(const Plan& lhs, const Plan& rhs);

};

}  // namespace attic
}  // namespace multio

#endif
