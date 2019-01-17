
#ifndef multio_server_Plan_H
#define multio_server_Plan_H

#include <memory>
#include <vector>

#include "Action.h"

namespace multio {
namespace server {

class Message;

class Plan {
public:
    explicit Plan(const std::string& nm, std::unique_ptr<Action>&& root);

    void process(const Message& msg) const;

private:  // members
    const std::string name_;
    std::unique_ptr<Action> root_;

private:  // methods
    void print(std::ostream&) const;
    friend std::ostream& operator<<(std::ostream& os, const Plan& plan) {
        plan.print(os);
        return os;
    }
};

}  // namespace server
}  // namespace multio

#endif
