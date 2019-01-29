
#ifndef multio_server_Dispatcher_H
#define multio_server_Dispatcher_H

#include <set>

#include "eckit/container/Queue.h"

#include "multio/server/Message.h"
#include "multio/server/Plan.h"
#include "multio/server/PlanAssembler.h"
#include "multio/server/Transport.h"

namespace multio {
namespace server {

class Dispatcher {
public:
    Dispatcher(const Transport& trans);

    void feedPlans(std::shared_ptr<Message> msg);

    void eventLoop();

private:  // members
    const Transport& transport_;

    std::set<Plan> registeredPlans_;

    eckit::Queue<std::shared_ptr<Message>> msgQueue_{1024};

    bool allPartsArrived_;

private:  // methods
    void listen();

    void dispatchNext();

    bool allPartsArrived(unsigned counter);

    void print(std::ostream& os) const;

    friend std::ostream& operator<<(std::ostream& os, const Dispatcher& dpatch) {
        dpatch.print(os);
        return os;
    }
};

}  // namespace server
}  // namespace multio

#endif
