
#ifndef multio_sandbox_Mappings_H
#define multio_sandbox_Mappings_H

#include <map>
#include <mutex>
#include <string>
#include <vector>

namespace multio {
namespace sandbox {

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

    mutable std::recursive_mutex mutex_;
};

}  // namespace sandbox
}  // namespace multio

#endif
