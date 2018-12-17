
#ifndef multio_server_Aggregation_H
#define multio_server_Aggregation_H

#include <unordered_map>
#include <vector>

#include "atlas/field/Field.h"

#include "Action.h"

namespace multio {
namespace server {

class Aggregation : public Action {
public:
    explicit Aggregation(std::vector<std::vector<int>> maps, const std::string& nm = "Aggregation");

private:  // methods
    void do_execute(const atlas::Field& field, int) const override;
    bool do_complete(atlas::Field& field) const override;

private:  // members
    struct GlobalField {
        unsigned no_chunks = 0;
        atlas::Field field;

        GlobalField() = default;
        GlobalField(atlas::Field fld);
    };
    const std::vector<std::vector<int>> mappings_;
    mutable std::unordered_map<std::string, GlobalField> globals_;
};

}  // namespace server
}  // namespace multio

#endif
