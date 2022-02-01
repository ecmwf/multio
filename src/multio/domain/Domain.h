
#ifndef multio_server_Domain_H
#define multio_server_Domain_H

#include <cstddef>
#include <cstdint>
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

    virtual void to_local(const std::vector<double>& global, std::vector<double>& local) const = 0;
    virtual void to_global(const message::Message& local, message::Message& global) const = 0;
    virtual void to_bitmask(const message::Message& local, std::vector<uint8_t>& bmask) const = 0;

protected:
    const std::vector<int32_t> definition_;  // Grid-point

};

class Unstructured final : public Domain {
public:
    Unstructured(std::vector<int32_t>&& def);

private:
    void to_local(const std::vector<double>& global, std::vector<double>& local) const override;
    void to_global(const message::Message& local, message::Message& global) const override;
    void to_bitmask(const message::Message& local, std::vector<uint8_t>& bmask) const override;
};

class Structured final : public Domain {
public:
    Structured(std::vector<int32_t>&& def);

private:
    void to_local(const std::vector<double>& global, std::vector<double>& local) const override;
    void to_global(const message::Message& local, message::Message& global) const override;
    void to_bitmask(const message::Message& local, std::vector<uint8_t>& bmask) const override;

    void checkDomainConsistency(const message::Message& local) const;
};

class Spectral final : public Domain {
public:
    Spectral(std::vector<int32_t>&& def);

private:
    void to_local(const std::vector<double>& global, std::vector<double>& local) const override;
    void to_global(const message::Message& local, message::Message& global) const override;
    void to_bitmask(const message::Message& local, std::vector<uint8_t>& bmask) const override;
};

}  // namespace domain
}  // namespace multio

#endif
