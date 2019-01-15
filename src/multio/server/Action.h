
#ifndef multio_server_Action_H
#define multio_server_Action_H

#include <memory>
#include <string>

namespace atlas {
class Field;
}

namespace multio {
namespace server {

class Action {
public:
    explicit Action(const std::string& nm);
    virtual ~Action() = default;

    Action* add(std::unique_ptr<Action>&& action);

    bool execute(atlas::Field& field, int source = -1) const;

private:  // methods

    virtual bool doExecute(atlas::Field& field, int source = -1) const = 0;

    void print(std::ostream&) const;
    friend std::ostream& operator<<(std::ostream& os, const Action& action);

private:  // members
    std::unique_ptr<Action> next_ = nullptr;
    const std::string name_;
};

}  // namespace server
}  // namespace multio

#endif
