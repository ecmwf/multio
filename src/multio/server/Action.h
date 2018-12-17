
#ifndef multio_server_Action_H
#define multio_server_Action_H

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

    void execute(const atlas::Field& field, int source = -1) const;
    bool complete(atlas::Field& field) const;

private:  // methods

    virtual void do_execute(const atlas::Field& field, int source = -1) const = 0;
    virtual bool do_complete(atlas::Field& field) const = 0;

    void print(std::ostream&) const;
    friend std::ostream& operator<<(std::ostream& os, const Action& action);

private:  // members

    const std::string name_;

};

}  // namespace server
}  // namespace multio

#endif
