
#ifndef multio_server_Distributor_H
#define multio_server_Distributor_H

#include <string>
#include <vector>

#include "multio/server/LocalPlan.h"
#include "multio/server/Transport.h"

namespace atlas {
class Field;
}

namespace multio {
namespace server {

class Distributor {
public:
    Distributor(const Transport& trans);

    size_t computeHash(const atlas::Field& field) const;

    void sendLocalPlan(const atlas::Field& field) const;
    void sendField(const atlas::Field& field) const;
    void sendForecastComplete() const;

private:
    mutable std::map<std::string, LocalPlan> distributed_plans;
    const Transport& transport_;

    void print(std::ostream &os) const;

    friend std::ostream& operator<<(std::ostream& os, const Distributor& distr) {
        distr.print(os);
        return os;
    }
};

}  // namespace server
}  // namespace multio

#endif
