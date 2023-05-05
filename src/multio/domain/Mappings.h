#pragma once

#include <algorithm>
#include <map>
#include <memory>
#include <mutex>
#include <string>
#include <vector>

#include "multio/domain/Domain.h"

#include "multio/message/Message.h"

namespace multio {
namespace domain {

class DomainMap {
public:
    std::unique_ptr<Domain>& at(const message::Peer& peer) { return domainMap_.at(peer); }

    const std::unique_ptr<Domain>& at(const message::Peer& peer) const { return domainMap_.at(peer); }

    bool contains(const message::Peer& peer) { return domainMap_.find(peer) != end(domainMap_); }

    template <typename... Args>
    void emplace(Args&&... args) {
        domainMap_.emplace(std::forward<Args>(args)...);
    }

    auto size() const -> std::map<message::Peer, std::unique_ptr<Domain>>::size_type {
        if (not isComplete()) {
            throw eckit::SeriousBug("Function size() is called before domain map is partially complete", Here());
        }

        return domainMap_.size();
    }

    bool isComplete() const {
        if (isConsistent()) {
            return true;
        }

        auto totalSize = 0;
        std::for_each(std::begin(domainMap_), std::end(domainMap_),
                      [&totalSize](const std::pair<const message::Peer, std::unique_ptr<Domain>>& domain) {
                          totalSize += domain.second->localSize();
                      });

        return (totalSize == domainMap_.begin()->second->partialSize());
    };

    bool isConsistent() const { return consistent_; }
    void isConsistent(bool val) const { consistent_ = val; }

private:
    std::map<message::Peer, std::unique_ptr<Domain>> domainMap_;
    mutable bool consistent_ = false;
};

class Mappings {
public:  // methods
    Mappings() = default;

    Mappings(const Mappings& rhs) = delete;
    Mappings(Mappings&& rhs) noexcept = delete;

    Mappings& operator=(const Mappings& rhs) = delete;
    Mappings& operator=(Mappings&& rhs) noexcept = delete;

    static Mappings& instance();

    void add(message::Message msg);

    void list(std::ostream&) const;

    const DomainMap& get(const std::string& name) const;

    void checkDomainConsistency(const std::vector<message::Message>& localDomains) const;

private:  // members
    std::map<std::string, DomainMap> mappings_;

    mutable std::recursive_mutex mutex_;
};

}  // namespace domain
}  // namespace multio
