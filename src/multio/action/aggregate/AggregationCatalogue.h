
#pragma once

#include <map>

#include "multio/message/Message.h"

namespace multio {
namespace action {

class AggregationCatalogue
{
public:
    message::Message& getMessage(const std::string& key);

    bool contains(const std::string& key) const;

    std::size_t partsCount(const std::string& key) const;

    void bookProcessedPart(const std::string& key, message::Peer peer);

    void addNew(const message::Message& msg);

    message::Message extract(const std::string& fid);

    std::size_t size() const;

private:
    void print(std::ostream& os) const;
    friend std::ostream& operator<<(std::ostream& os, const AggregationCatalogue& a);

    std::map<std::string, message::Message> messageMap_;
    std::map<std::string, std::set<message::Peer>> processedParts_;
};

}  // namespace action
}  // namespace multio
