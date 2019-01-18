
#ifndef multio_server_Aggregation_H
#define multio_server_Aggregation_H

#include <unordered_map>
#include <vector>

#include "atlas/field/Field.h"

#include "multio/server/Action.h"
#include "multio/server/Message.h"

namespace multio {
namespace server {

class Message;

class Aggregation : public Action {
public:
    explicit Aggregation(std::vector<std::vector<int>> maps, const std::string& nm = "Aggregation");

private:  // methods
    bool doExecute(std::shared_ptr<Message> msg) const override;
    atlas::Field aggregate(const std::string& meta_str) const;

private:  // members
    const std::vector<std::vector<int>> mappings_;
    mutable std::unordered_map<std::string, std::vector<std::shared_ptr<Message>>> messages_;
};

}  // namespace server
}  // namespace multio

#endif
