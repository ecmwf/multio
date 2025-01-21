
#include "AggregationCatalogue.h"

#include "multio/domain/Mappings.h"

#include "multio/message/Parametrization.h"

namespace multio::action {

message::Message& AggregationCatalogue::getMessage(const std::string& key) {
    return messageMap_.at(key);
}

bool AggregationCatalogue::contains(const std::string& key) const {
    return (messageMap_.find(key) != messageMap_.end()) && (processedParts_.find(key) != std::end(processedParts_));
}

std::size_t AggregationCatalogue::partsCount(const std::string& key) const {
    ASSERT(contains(key));
    return processedParts_.at(key).size();
}

void AggregationCatalogue::bookProcessedPart(const std::string& key, message::Peer peer) {
    ASSERT(contains(key));
    auto ret = processedParts_.at(key).insert(std::move(peer));
    if (not ret.second) {
        eckit::Log::warning() << " Field " << key << " has been aggregated already" << std::endl;
    }
}

void AggregationCatalogue::addNew(const message::Message& msg) {
    ASSERT(not contains(msg.fieldId()));
    multio::util::dispatchPrecisionTag(msg.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        // Create a new message with preallocated  payload where individual arriving parts are immediately copied in.
        // The source of the message is the same as the destination - otherwise on serverside the source is depending on
        // the source of the first arriving although all multiple sources are combined on the servier (which is the
        // destination).
        messageMap_.emplace(
            msg.fieldId(),
            message::Message{message::Message::Header{msg.header().tag(), msg.header().destination(),
                                                      msg.header().destination(), msg.header().moveOrCopyMetadata()},
                             eckit::Buffer{msg.globalSize() * sizeof(Precision)}});
        processedParts_.emplace(msg.fieldId(), std::set<message::Peer>{});
    });
}

message::Message AggregationCatalogue::extract(const std::string& fid) {
    auto it = messageMap_.find(fid);
    ASSERT(it != end(messageMap_));

    auto msgOut = std::move(it->second);

    processedParts_.erase(it->first);
    messageMap_.erase(it);

    return msgOut;
}

std::size_t AggregationCatalogue::size() const {
    // Invariant
    ASSERT(messageMap_.size() == processedParts_.size());

    return messageMap_.size();
}

void AggregationCatalogue::print(std::ostream& os) const {
    for (const auto& mp : messageMap_) {
        auto const& domainMap = domain::Mappings::instance().get(mp.second.domain());
        os << '\n'
           << "  --->  " << mp.first << " ---> Aggregated " << partsCount(mp.first) << " parts of a total of "
           << (domainMap.isComplete() ? domainMap.size() : 0);
    }
}

std::ostream& operator<<(std::ostream& os, const AggregationCatalogue& aggCat) {
    aggCat.print(os);
    return os;
}

}  // namespace multio::action
