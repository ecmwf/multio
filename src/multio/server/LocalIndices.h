
#ifndef multio_server_LocalIndices_H
#define multio_server_LocalIndices_H

#include <cstddef>
#include <cstdint>
#include <vector>

namespace multio {
namespace server {

class LocalIndices {
public:
    LocalIndices(std::vector<int32_t>&& idx);

    void to_global(const std::vector<double>& local, std::vector<double>& global) const;
    void to_local(const std::vector<double>& global, std::vector<double>& local) const;

private:
    std::vector<int32_t> indices_;  // Grid-point
};

}  // namespace server
}  // namespace multio

#endif
