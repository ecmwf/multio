
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

    virtual void to_global(const std::vector<double>& local, std::vector<double>& global) const = 0;
    virtual void to_local(const std::vector<double>& global, std::vector<double>& local) const = 0;

protected:
    std::vector<int32_t> indices_;  // Grid-point

};

class Unstructured final : public LocalIndices {
public:
    Unstructured(std::vector<int32_t>&& idx);

private:
    void to_global(const std::vector<double>& local, std::vector<double>& global) const override;
    void to_local(const std::vector<double>& global, std::vector<double>& local) const override;
};

class Structured final : public LocalIndices {
public:
    Structured(std::vector<int32_t>&& idx);

private:
    void to_global(const std::vector<double>& local, std::vector<double>& global) const override;
    void to_local(const std::vector<double>& global, std::vector<double>& local) const override;
};

class Spectral final : public LocalIndices {
public:
    Spectral(std::vector<int32_t>&& idx);

private:
    void to_global(const std::vector<double>& local, std::vector<double>& global) const override;
    void to_local(const std::vector<double>& global, std::vector<double>& local) const override;
};

}  // namespace server
}  // namespace multio

#endif
