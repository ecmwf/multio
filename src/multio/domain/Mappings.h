
#ifndef multio_server_Mappings_H
#define multio_server_Mappings_H

#include <map>
#include <memory>
#include <mutex>
#include <string>
#include <vector>

#include "multio/domain/Domain.h"

namespace multio {

class Message;
class Peer;

namespace domain {

using Mapping = std::map<Peer, std::unique_ptr<Domain>>;

class Mappings {
public:  // methods
    Mappings() = default;

    Mappings(const Mappings& rhs) = delete;
    Mappings(Mappings&& rhs) noexcept = delete;

    Mappings& operator=(const Mappings& rhs) = delete;
    Mappings& operator=(Mappings&& rhs) noexcept = delete;

    static Mappings& instance();

    void add(Message msg);

    void list(std::ostream&) const;

    const Mapping& get(const std::string& name) const;

private:  // members
    std::map<std::string, Mapping> mappings_;

    mutable std::recursive_mutex mutex_;
};

}  // namespace domain
}  // namespace multio

#endif
