#ifndef multio_attic_Select_H
#define multio_attic_Select_H

#include <vector>

#include "Action.h"

namespace multio {
namespace attic {

class Message;

class Select : public Action {
public:
    explicit Select(const std::vector<std::string>& ctgs, const std::string& nm = "Select");

private:  // methods
    bool doExecute(std::shared_ptr<Message> msg) const override;

    bool matchPlan(const Message& msg) const;

private:  // members
    std::vector<std::string> categories_;
};

}  // namespace attic
}  // namespace multio

#endif
