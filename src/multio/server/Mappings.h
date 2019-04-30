
#ifndef multio_server_Mappings_H
#define multio_server_Mappings_H

#include <map>
#include <mutex>
#include <string>
#include <vector>

namespace multio {
namespace server {

class LocalIndices;
class Message;
class Peer;

using Mapping = std::map<Peer, LocalIndices>;

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

}  // namespace server
}  // namespace multio

#endif
