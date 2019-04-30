
#ifndef multio_attic_Distributor_H
#define multio_attic_Distributor_H

#include <string>
#include <vector>
#include <map>

#include "multio/attic/msg_tag.h"
#include "multio/attic/PartialMapping.h"
#include "multio/attic/Transport.h"

namespace atlas {
class Field;
}

namespace multio {
namespace attic {

class Distributor {
public:
    Distributor(const Transport& trans);

    void sendPartialMapping(const atlas::Field& field) const;
    void sendPartialField(const atlas::Field& field) const;
    void sendNotification(const msg_tag notification) const;

private:  // members
    mutable std::map<std::string, PartialMapping> distributed_mappings;
    const Transport& transport_;

private:  // methods
    size_t computeHash(const atlas::Field& field) const;

    void print(std::ostream& os) const;

    friend std::ostream& operator<<(std::ostream& os, const Distributor& distr) {
        distr.print(os);
        return os;
    }
};

}  // namespace attic
}  // namespace multio

#endif
