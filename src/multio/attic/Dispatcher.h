
#ifndef multio_attic_Dispatcher_H
#define multio_attic_Dispatcher_H

#include <atomic>
#include <set>

#include "eckit/container/Queue.h"

#include "multio/attic/Message.h"
#include "multio/attic/Plan.h"
#include "multio/attic/PlanAssembler.h"
#include "multio/attic/Transport.h"

namespace multio {
namespace attic {

class Dispatcher {
public:
    Dispatcher(const Transport& trans);

    void feedPlans(std::shared_ptr<Message> msg);

    void eventLoop();

private:  // members
    const Transport& transport_;

    std::set<Plan> registeredPlans_;

    eckit::Queue<std::shared_ptr<Message>> msgQueue_{1024};

    std::atomic<unsigned> counter_{0};

private:  // methods
    void listen();

    void dispatchNext();

    bool allPartsArrived();

    void print(std::ostream& os) const;

    friend std::ostream& operator<<(std::ostream& os, const Dispatcher& dpatch) {
        dpatch.print(os);
        return os;
    }
};

}  // namespace attic
}  // namespace multio

#endif
