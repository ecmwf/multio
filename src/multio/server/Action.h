
#ifndef multio_server_Action_H
#define multio_server_Action_H

#include <memory>
#include <string>

namespace multio {
namespace server {

class Message;

class Action {
public:
    explicit Action(const std::string& nm);
    virtual ~Action() = default;

    Action* add(std::unique_ptr<Action>&& action);

    bool execute(Message& msg) const;

private:  // methods

    virtual bool doExecute(Message& msg) const = 0;

    void print(std::ostream&) const;
    friend std::ostream& operator<<(std::ostream& os, const Action& action);

private:  // members
    std::unique_ptr<Action> next_ = nullptr;
    const std::string name_;
};

}  // namespace server
}  // namespace multio

#endif
