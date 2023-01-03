#ifndef multio_server_actions_Operation_H
#define multio_server_actions_Operation_H

#include <eckit/exception/Exceptions.h>
#include <eckit/io/Buffer.h>
#include <multio/util/VariantHelpers.h>

#include <memory>
#include <string>
#include <vector>

namespace multio {
namespace action {

//==== Base class =================================

template <typename T, typename = std::enable_if_t<std::is_floating_point<T>::value>>
class Operation {
public:
    Operation(const std::string& name, long sz) : name_{name}, values_{std::vector<T>(sz /= sizeof(T))} {}
    const std::string& name() { return name_; }

    virtual eckit::Buffer compute() = 0;
    virtual void update(const void* val, long sz) = 0;

    virtual ~Operation() = default;

protected:
    virtual void print(std::ostream& os) const = 0;

    std::string name_;
    std::vector<T> values_;

    friend std::ostream& operator<<(std::ostream& os, const Operation& a) {
        a.print(os);
        return os;
    }
};


using OperationVar = std::variant<std::unique_ptr<Operation<double>>, std::unique_ptr<Operation<float>>>;


//==== Derived classes ============================
template <typename T>
class Instant final : public Operation<T> {
public:
    using Operation<T>::values_;

    Instant(const std::string& name, long sz = 0) : Operation<T>{name, sz} {}

    eckit::Buffer compute() override { return eckit::Buffer{values_.data(), values_.size() * sizeof(T)}; }

    void update(const void* data, long sz) override {
        auto val = static_cast<const T*>(data);
        sz /= sizeof(T);

        ASSERT(values_.size() == static_cast<size_t>(sz));

        // May never be needed -- just creates an unnecessarily copy
        std::copy(val, val + sz, values_.begin());
    }

private:
    void print(std::ostream& os) const override { os << "Operation(instant)"; }
};

template <typename T>
class Average final : public Operation<T> {
    long count_ = 0;

public:
    using Operation<T>::values_;

    Average(const std::string& name, long sz = 0) : Operation<T>{name, sz} {}

    eckit::Buffer compute() override {

        for (auto& val : values_) {
            val /= static_cast<T>(count_);
        }

        return eckit::Buffer{values_.data(), values_.size() * sizeof(T)};
    }

    void update(const void* data, long sz) override {
        auto val = static_cast<const T*>(data);
        sz /= sizeof(T);

        if (values_.size() != static_cast<size_t>(sz)) {
            throw eckit::AssertionFailed("Expected size: " + std::to_string(values_.size())
                                         + " -- actual size: " + std::to_string(sz));
        }

        for (auto& v : values_) {
            v += *val++;
        }
        ++count_;
    }

private:
    void print(std::ostream& os) const override { os << "Operation(average)"; }
};


template <typename T>
class Minimum final : public Operation<T> {
public:
    using Operation<T>::values_;

    Minimum(const std::string& name, long sz = 0) : Operation<T>{name, sz} {}

    eckit::Buffer compute() override { return eckit::Buffer{values_.data(), values_.size() * sizeof(T)}; }

    void update(const void* data, long sz) override {
        auto val = static_cast<const T*>(data);
        sz /= sizeof(T);

        ASSERT(values_.size() == static_cast<size_t>(sz));

        for (auto& v : values_) {
            v = (v > *val) ? *val : v;
            ++val;
        }
    }

private:
    void print(std::ostream& os) const override { os << "Operation(minimum)"; }
};


template <typename T>
class Maximum final : public Operation<T> {
public:
    using Operation<T>::values_;

    Maximum(const std::string& name, long sz = 0) : Operation<T>{name, sz} {}

    eckit::Buffer compute() override { return eckit::Buffer{values_.data(), values_.size() * sizeof(T)}; }

    void update(const void* data, long sz) override {
        auto val = static_cast<const T*>(data);
        sz /= sizeof(T);

        ASSERT(values_.size() == static_cast<size_t>(sz));

        for (auto& v : values_) {
            v = (v < *val) ? *val : v;
            ++val;
        }
    }


private:
    void print(std::ostream& os) const override { os << "Operation(maximum)"; }
};

template <typename T>
class Accumulate final : public Operation<T> {
public:
    using Operation<T>::values_;

    Accumulate(const std::string& name, long sz = 0) : Operation<T>{name, sz} {};

    eckit::Buffer compute() override { return eckit::Buffer{values_.data(), values_.size() * sizeof(T)}; }

    void update(const void* data, long sz) override {
        auto val = static_cast<const T*>(data);
        sz /= sizeof(T);

        ASSERT(values_.size() == static_cast<size_t>(sz));

        for (auto& v : values_) {
            v += *val++;
        }
    }

private:
    void print(std::ostream& os) const override { os << "Operation(accumulate)"; }
};

//==== Factory function ============================
template <typename T>
std::unique_ptr<Operation<T>> make_operation(const std::string& opname, long sz) {

    if (opname == "instant") {
        return std::make_unique<Instant<T>>(opname, sz);
    }
    if (opname == "average") {
        return std::make_unique<Average<T>>(opname, sz);
    }
    if (opname == "minimum") {
        return std::make_unique<Minimum<T>>(opname, sz);
    }
    if (opname == "maximum") {
        return std::make_unique<Maximum<T>>(opname, sz);
    }
    ASSERT(opname == "accumulate");
    return std::make_unique<Accumulate<T>>(opname, sz);
}

}  // namespace action
}  // namespace multio

#endif  // multio_server_actions_Operation_H
