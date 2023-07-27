#pragma once

#include <cstddef>
#include <cstdint>
#include <set>
#include <vector>

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

    virtual std::int64_t localSize() const = 0;
    virtual std::int64_t globalSize() const = 0;
    virtual std::int64_t partialSize() const = 0;

    virtual void collectIndices(const message::Message& local, std::set<int32_t>& glIndices) const = 0;

protected:
    const std::vector<int32_t> definition_;  // Grid-point
};

class Unstructured final : public Domain {
public:
    Unstructured(std::vector<int32_t>&& def, std::int64_t global_size);

private:
    void toLocal(const std::vector<double>& global, std::vector<double>& local) const override;
    void toGlobal(const message::Message& local, message::Message& global) const override;
    void toBitmask(const message::Message& local, std::vector<bool>& bmask) const override;

    std::int64_t localSize() const override;
    std::int64_t globalSize() const override;
    std::int64_t partialSize() const override;

    void collectIndices(const message::Message& local, std::set<int32_t>& glIndices) const override;

    template <typename Precision>
    void toGlobalImpl(const message::Message& local, message::Message& global) const;

    std::int64_t globalSize_;
};

class Structured final : public Domain {
public:
    Structured(std::vector<int32_t>&& def);

private:
    void toLocal(const std::vector<double>& global, std::vector<double>& local) const override;
    void toGlobal(const message::Message& local, message::Message& global) const override;
    void toBitmask(const message::Message& local, std::vector<bool>& bmask) const override;

    std::int64_t localSize() const override;
    std::int64_t globalSize() const override;
    std::int64_t partialSize() const override;

    void collectIndices(const message::Message& local, std::set<int32_t>& glIndices) const override;

    template <typename Precision>
    void toGlobalImpl(const message::Message& local, message::Message& global) const;

    template <typename Precision>
    void collectIndicesImpl(const message::Message& local, std::set<int32_t>& glIndices) const;
};

class Spectral final : public Domain {
public:
    Spectral(std::vector<int32_t>&& def);

private:
    void toLocal(const std::vector<double>& global, std::vector<double>& local) const override;
    void toGlobal(const message::Message& local, message::Message& global) const override;
    void toBitmask(const message::Message& local, std::vector<bool>& bmask) const override;

    std::int64_t localSize() const override;
    std::int64_t globalSize() const override;
    std::int64_t partialSize() const override;

    void collectIndices(const message::Message& local, std::set<int32_t>& glIndices) const override;
};

}  // namespace domain
}  // namespace multio
