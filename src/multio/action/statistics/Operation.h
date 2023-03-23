#pragma once

#include <memory>
#include <string>
#include <variant>
#include <vector>

#include "eckit/exception/Exceptions.h"
#include "eckit/io/Buffer.h"
#include "eckit/log/Log.h"

#include "multio/LibMultio.h"
#include "multio/action/statistics/StatisticsOptions.h"
#include "multio/util/VariantHelpers.h"

namespace multio {
namespace action {

//==== Base class =================================

template <typename T, typename = std::enable_if_t<std::is_floating_point<T>::value>>
class Operation {
public:
    Operation(const std::string& name, const std::string& operation, long sz, const StatisticsOptions& options) :
        name_{name}, operation_{operation}, values_{std::vector<T>(sz /= sizeof(T), 0.0)}, options_{options} {}
    const std::string& name() { return name_; }
    const std::string& operation() { return operation_; }

    virtual eckit::Buffer compute() = 0;
    virtual void update(const void* val, long sz) = 0;

    virtual ~Operation() = default;

protected:
    virtual void print(std::ostream& os) const = 0;

    std::string name_;
    std::string operation_;
    std::vector<T> values_;
    const StatisticsOptions& options_;

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
    using Operation<T>::options_;

    Instant(const std::string& name, long sz, const StatisticsOptions& options) :
        Operation<T>{name, "instant", sz, options} {}

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
    using Operation<T>::options_;

    Average(const std::string& name, long sz, const StatisticsOptions& options) :
        Operation<T>{name, "average", sz, options} {}

    eckit::Buffer compute() override { return eckit::Buffer{values_.data(), values_.size() * sizeof(T)}; }

    void update(const void* data, long sz) override {
        auto val = static_cast<const T*>(data);
        sz /= sizeof(T);

        if (values_.size() != static_cast<size_t>(sz)) {
            throw eckit::AssertionFailed("Expected size: " + std::to_string(values_.size())
                                         + " -- actual size: " + std::to_string(sz));
        }

        try {
            // Compute the running average in order to avoid precison problems
            // TODO: the scale factor can be computed using eckit::fraction
            // TODO: Handling missing values
            T cntpp = static_cast<T>(count_ + 1);
            T sc = static_cast<T>(count_) / cntpp;
            for (auto& v : values_) {
                v = v * sc + (*val++) / cntpp;
            }
            ++count_;
        }
        catch (...) {
            LOG_DEBUG_LIB(LibMultio) << "ACCUMULATED DATA" << std::endl;
            for (auto& v : values_) {
                LOG_DEBUG_LIB(LibMultio) << v << ", ";
            }
            LOG_DEBUG_LIB(LibMultio) << std::endl;
            LOG_DEBUG_LIB(LibMultio) << "NEW DATA" << std::endl;
            val = static_cast<const T*>(data);
            for (auto& v : values_) {
                LOG_DEBUG_LIB(LibMultio) << val++ << ", ";
            }
            LOG_DEBUG_LIB(LibMultio) << std::endl;
            throw eckit::SeriousBug("numerical error dureing average update", Here());
        }
    }

private:
    void print(std::ostream& os) const override { os << "Operation(average)"; }
};


template <typename T>
class WeightedAverage final : public Operation<T> {
    long count_ = 0;

public:
    using Operation<T>::values_;
    using Operation<T>::options_;

    WeightedAverage(const std::string& name, long sz, const StatisticsOptions& options) :
        Operation<T>{name, "average", sz, options} {}

    eckit::Buffer compute() override {
        // TODO: handle
        for (auto& val : values_) {
            val /= static_cast<T>(count_ * options_.stepFreq());
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

        // Integration is done by considering the current step constant for the last stepFreq steps
        for (auto& v : values_) {
            v += (*val++) * options_.stepFreq();
        }
        ++count_;
    }

private:
    void print(std::ostream& os) const override { os << "Operation(weighted-average)"; }
};


template <typename T>
class FieldAverage final : public Operation<T> {
    long count_ = 0;

public:
    using Operation<T>::values_;
    using Operation<T>::options_;

    FieldAverage(const std::string& name, long sz, const StatisticsOptions& options) :
        Operation<T>{name, "average", sz, options} {}

    eckit::Buffer compute() override {

        // TODO: take care of the missing values
        for (auto& val : values_) {
            val /= static_cast<T>(count_ * options_.stepFreq() * options_.timeStep());
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
        // May never be needed -- just creates an unnecessarily copy
        std::copy(val, val + sz, values_.begin());
        ++count_;
    }

private:
    void print(std::ostream& os) const override { os << "Operation(field-average)"; }
};


template <typename T>
class Minimum final : public Operation<T> {
public:
    using Operation<T>::values_;
    using Operation<T>::options_;

    Minimum(const std::string& name, long sz, const StatisticsOptions& options) :
        Operation<T>{name, "minimum", sz, options} {}

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
    using Operation<T>::options_;

    Maximum(const std::string& name, long sz, const StatisticsOptions& options) :
        Operation<T>{name, "maximum", sz, options} {}

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
    using Operation<T>::options_;

    Accumulate(const std::string& name, long sz, const StatisticsOptions& options) :
        Operation<T>{name, "accumulate", sz, options} {};

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
std::unique_ptr<Operation<T>> make_operation(const std::string& opname, long sz, const StatisticsOptions& options) {

    if (opname == "instant") {
        return std::make_unique<Instant<T>>(opname, sz, options);
    }
    if (opname == "average") {
        return std::make_unique<Average<T>>(opname, sz, options);
    }
    if (opname == "weighted-average") {
        return std::make_unique<WeightedAverage<T>>(opname, sz, options);
    }
    if (opname == "field-average") {
        return std::make_unique<FieldAverage<T>>(opname, sz, options);
    }
    if (opname == "minimum") {
        return std::make_unique<Minimum<T>>(opname, sz, options);
    }
    if (opname == "maximum") {
        return std::make_unique<Maximum<T>>(opname, sz, options);
    }
    ASSERT(opname == "accumulate");
    return std::make_unique<Accumulate<T>>(opname, sz, options);
}

}  // namespace action
}  // namespace multio
