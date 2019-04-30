
#ifndef multio_attic_Aggregation_H
#define multio_attic_Aggregation_H

#include <unordered_map>
#include <vector>

#include "atlas/field/Field.h"

#include "multio/attic/Action.h"
#include "multio/attic/Message.h"

namespace multio {
namespace attic {

class Message;

class Aggregation : public Action {
public:
    explicit Aggregation(const std::string& nm = "Aggregation");

private:  // methods
    bool doExecute(std::shared_ptr<Message> msg) const override;
    atlas::Field aggregate(const std::string& meta_str) const;

private:  // members
    // const std::vector<std::vector<int>> mappings_;
    mutable std::string map_name_;
    mutable std::unordered_map<std::string, std::vector<std::shared_ptr<Message>>> messages_;
};

}  // namespace attic
}  // namespace multio

#endif
