#pragma once

#include <cstddef>
#include <cstdint>
#include <set>
#include <vector>

#include <eckit/io/Buffer.h>

namespace multio {

namespace message {
class Message;
}

namespace domain {

class Domain {
public:
    Domain(std::vector<int32_t>&& def);
    virtual ~Domain() = default;

    virtual void toLocal(const std::vector<double>& global, std::vector<double>& local) const = 0;
    virtual void toGlobal(const message::Message& local, message::Message& global) const = 0;
    virtual void toBitmask(const message::Message& local, std::vector<bool>& bmask) const = 0;

    virtual long localSize() const = 0;
    virtual long globalSize() const = 0;

    virtual void collectIndices(const message::Message& local, std::set<int32_t>& glIndices) const = 0;

protected:
    const std::vector<int32_t> definition_;  // Grid-point
};

class Unstructured final : public Domain {
public:
    Unstructured(std::vector<int32_t>&& def, long global_size);

private:
    void toLocal(const std::vector<double>& global, std::vector<double>& local) const override;
    void toGlobal(const message::Message& local, message::Message& global) const override;
    void toBitmask(const message::Message& local, std::vector<bool>& bmask) const override;

    long localSize() const override;
    long globalSize() const override;

    void collectIndices(const message::Message& local, std::set<int32_t>& glIndices) const override;

    long globalSize_;
};

class Structured final : public Domain {
public:
    Structured(std::vector<int32_t>&& def);

private:
    void toLocal(const std::vector<double>& global, std::vector<double>& local) const override;
    void toGlobal(const message::Message& local, message::Message& global) const override;
    void toBitmask(const message::Message& local, std::vector<bool>& bmask) const override;

    long localSize() const override;
    long globalSize() const override;

    void collectIndices(const message::Message& local, std::set<int32_t>& glIndices) const override;
};

class Spectral final : public Domain {
public:
    Spectral(std::vector<int32_t>&& def);

private:
    void toLocal(const std::vector<double>& global, std::vector<double>& local) const override;
    void toGlobal(const message::Message& local, message::Message& global) const override;
    void toBitmask(const message::Message& local, std::vector<bool>& bmask) const override;

    long localSize() const override;
    long globalSize() const override;

    void collectIndices(const message::Message& local, std::set<int32_t>& glIndices) const override;
};

}  // namespace domain
}  // namespace multio
