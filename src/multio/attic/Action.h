
#ifndef multio_attic_Action_H
#define multio_attic_Action_H

#include <memory>
#include <string>

namespace multio {
namespace attic {

class Message;

class Action {
public:
    explicit Action(const std::string& nm);
    virtual ~Action() = default;

    Action* add(std::unique_ptr<Action>&& action);

    bool execute(std::shared_ptr<Message> msg) const;

private:  // methods

    virtual bool doExecute(std::shared_ptr<Message> msg) const = 0;

    void print(std::ostream&) const;
    friend std::ostream& operator<<(std::ostream& os, const Action& action);

private:  // members
    std::unique_ptr<Action> next_ = nullptr;
    const std::string name_;
};

}  // namespace attic
}  // namespace multio

#endif
