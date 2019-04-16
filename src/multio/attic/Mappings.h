
#ifndef multio_attic_Mappings_H
#define multio_attic_Mappings_H

#include <map>
#include <mutex>
#include <string>
#include <vector>

namespace multio {
namespace attic {

class Message;

class Mappings {
    using LocalIndices = std::vector<int>;
    using Mapping = std::vector<LocalIndices>;

public:  // methods
    Mappings() = default;

    Mappings(const Mappings& rhs) = delete;
    Mappings(Mappings&& rhs) noexcept = delete;

    Mappings& operator=(const Mappings& rhs) = delete;
    Mappings& operator=(Mappings&& rhs) noexcept = delete;

    static Mappings& instance();

    void add(const Message& msg);

    void list(std::ostream&) const;

    const Mapping& get(const std::string& name) const;

private:  // members
    std::map<std::string, Mapping> mappings_;

    mutable std::mutex mutex_;
};

}  // namespace attic
}  // namespace multio

#endif
