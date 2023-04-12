#pragma once

#include <fstream>
#include <iostream>
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
    virtual void dump(const std::string& partialPath, bool noThrow) const = 0;

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

    Instant(const std::string& name, long sz, const std::string& partialPath, const StatisticsOptions& options) :
        Operation<T>{name, "instant", sz, options} {
#if 0
        std::ostringstream os;
        os << partialPath << "-instant-data.bin";
        std::string fname = os.str();
        std::ifstream wf(fname, std::ios::binary);
        if (!wf) {
            throw eckit::SeriousBug("Cannot open file!", Here());
        }
        long dim;
        // wf.write((char *) &count_, sizeof(long));
        wf.read((char*)&dim, sizeof(long));
        values_.resize(dim);
        for (int i = 0; i < dim; ++i) {
            double tmp;
            wf.read((char*)&tmp, sizeof(double));
            values_[i] = static_cast<T>(tmp);
        }
        wf.close();
        if (!wf.good()) {
            throw eckit::SeriousBug("Error occurred at writing time!", Here());
        }
#endif
        return;
    };

    void dump(const std::string& partialPath, bool noThrow) const override {
#if 0
        std::ostringstream os;
        os << partialPath << "-insatant-data.bin";
        std::string fname = os.str();
        std::ofstream wf(fname, std::ios::binary);
        if (!wf) {
            if (noThrow) {
                LOG_DEBUG_LIB(LibMultio) << "Cannot open dump file: fname" << std::endl;
            }
            else {
                throw eckit::SeriousBug("Cannot open file!", Here());
            }
        }
        long sz = values_.size();
        // wf.write((char *) &count_, sizeof(long));
        wf.write((char*)&sz, sizeof(long));
        for (int i = 0; i < sz; ++i) {
            double tmp = double(values_[i]);
            wf.write((char*)&tmp, sizeof(double));
        }
        wf.close();
        if (!wf.good()) {
            if (noThrow) {
                LOG_DEBUG_LIB(LibMultio) << "Error occurred at writing time: fname" << std::endl;
            }
            else {
                throw eckit::SeriousBug("Error occurred at writing time!", Here());
            }
        }
#endif
    }

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
    using Operation<T>::name_;
    using Operation<T>::values_;
    using Operation<T>::options_;

    Average(const std::string& name, long sz, const StatisticsOptions& options) :
        Operation<T>{name, "average", sz, options} {}

    Average(const std::string& name, long sz, const std::string& partialPath, const StatisticsOptions& options) :
        Operation<T>{name, "average", sz, options} {
        std::ostringstream os;
        os << partialPath << "-average-data.bin";
        std::string fname = os.str();
        std::ifstream wf(fname, std::ios::binary);
        if (!wf) {
            throw eckit::SeriousBug("Cannot open file!", Here());
        }
        long dim;
        wf.read((char*)&count_, sizeof(long));
        wf.read((char*)&dim, sizeof(long));
        if (dim != sz / sizeof(T)) {
            std::ostringstream err;
            err << "Wrong size during restart of average-statistics :: " << dim << ", " << sz;
            throw eckit::SeriousBug(err.str(), Here());
        }
        LOG_DEBUG_LIB(LibMultio) << "The counter is :: " << count_ << std::endl;
        values_.resize(dim);
        for (int i = 0; i < dim; ++i) {
            double tmp;
            wf.read((char*)&tmp, sizeof(double));
            values_[i] = static_cast<T>(tmp);
        }
        wf.close();
        if (!wf.good()) {
            throw eckit::SeriousBug("Error occurred at writing time!", Here());
        }
        return;
    };

    void dump(const std::string& partialPath, bool noThrow) const override  {
        std::ostringstream os;
        os << partialPath << "-average-data.bin";
        std::string fname = os.str();
        std::ofstream wf(fname, std::ios::binary);
        if (!wf) {
            if (noThrow) {
                LOG_DEBUG_LIB(LibMultio) << "Cannot open dump file: fname" << std::endl;
            }
            else {
                throw eckit::SeriousBug("Cannot open file!", Here());
            }
        }
        long sz = values_.size();
        wf.write((char*)&count_, sizeof(long));
        wf.write((char*)&sz, sizeof(long));
        for (int i = 0; i < sz; ++i) {
            double tmp = double(values_[i]);
            wf.write((char*)&tmp, sizeof(double));
        }
        wf.close();
        if (!wf.good()) {
            if (noThrow) {
                LOG_DEBUG_LIB(LibMultio) << "Error occurred at writing time: fname" << std::endl;
            }
            else {
                throw eckit::SeriousBug("Error occurred at writing time!", Here());
            }
        }
    }


    eckit::Buffer compute() override {
        LOG_DEBUG_LIB(LibMultio) << "statistics (" << name_ << ") compute :: count=" << count_ << std::endl;
        return eckit::Buffer{values_.data(), values_.size() * sizeof(T)};
    }

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
            double icntpp = double(1.0) / double(count_ + 1);
            double sc = double(count_) * icntpp;
            for (auto& v : values_) {
                v = v * sc + (*val++) * icntpp;
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
            throw eckit::SeriousBug("numerical error during average update", Here());
        }
    }

private:
    void print(std::ostream& os) const override { os << "Operation(average)"; }
};

template <typename T>
class FluxAverage final : public Operation<T> {
    long count_ = 0;

public:
    using Operation<T>::name_;
    using Operation<T>::values_;
    using Operation<T>::options_;

    FluxAverage(const std::string& name, long sz, const StatisticsOptions& options) :
        Operation<T>{name, "average", sz, options} {}

    FluxAverage(const std::string& name, long sz, const std::string& partialPath, const StatisticsOptions& options) :
        Operation<T>{name, "average", sz, options} {
        std::ostringstream os;
        os << partialPath << "-flux-average-data.bin";
        std::string fname = os.str();
        std::ifstream wf(fname, std::ios::binary);
        if (!wf) {
            throw eckit::SeriousBug("Cannot open file!", Here());
        }
        long dim;
        wf.read((char*)&count_, sizeof(long));
        wf.read((char*)&dim, sizeof(long));
        if (dim != sz / sizeof(T)) {
            throw eckit::SeriousBug("Wrong size during restart of flux-average-statistics", Here());
        }
        values_.resize(dim);
        for (int i = 0; i < dim; ++i) {
            double tmp;
            wf.read((char*)&tmp, sizeof(double));
            values_[i] = static_cast<T>(tmp);
        }
        wf.close();
        if (!wf.good()) {
            throw eckit::SeriousBug("Error occurred at writing time!", Here());
        }
        return;
    };

    void dump(const std::string& partialPath, bool noThrow) const override  {
        std::ostringstream os;
        os << partialPath << "-flux-average-data.bin";
        std::string fname = os.str();
        std::ofstream wf(fname, std::ios::binary);
        if (!wf) {
            if (noThrow) {
                LOG_DEBUG_LIB(LibMultio) << "Cannot open dump file: fname" << std::endl;
            }
            else {
                throw eckit::SeriousBug("Cannot open file!", Here());
            }
        }
        long sz = values_.size();
        wf.write((char*)&count_, sizeof(long));
        wf.write((char*)&sz, sizeof(long));
        for (int i = 0; i < sz; ++i) {
            double tmp = double(values_[i]);
            wf.write((char*)&tmp, sizeof(double));
        }
        wf.close();
        if (!wf.good()) {
            if (noThrow) {
                LOG_DEBUG_LIB(LibMultio) << "Error occurred at writing time: fname" << std::endl;
            }
            else {
                throw eckit::SeriousBug("Error occurred at writing time!", Here());
            }
        }
    }


    eckit::Buffer compute() override {

        // TODO: take care of the missing values
        for (auto& val : values_) {
            val /= static_cast<T>(count_ * options_.stepFreq() * options_.timeStep());
        }
        LOG_DEBUG_LIB(LibMultio) << "statistics (" << name_ << ") compute :: count=" << count_ << std::endl;
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
    void print(std::ostream& os) const override { os << "Operation(flux-average)"; }
};


template <typename T>
class Minimum final : public Operation<T> {
public:
    using Operation<T>::values_;
    using Operation<T>::options_;

    Minimum(const std::string& name, long sz, const StatisticsOptions& options) :
        Operation<T>{name, "minimum", sz, options} {}

    Minimum(const std::string& name, long sz, const std::string& partialPath, const StatisticsOptions& options) :
        Operation<T>{name, "minimum", sz, options} {
#if 0
        std::ostringstream os;
        os << partialPath << "-minimum-data.bin";
        std::string fname = os.str();
        std::ifstream wf(fname, std::ios::binary);
        if (!wf) {
            throw eckit::SeriousBug("Cannot open file!", Here());
        }
        long dim;
        // wf.write((char *) &count_, sizeof(long));
        wf.read((char*)&dim, sizeof(long));
        values_.resize(dim);
        for (int i = 0; i < dim; ++i) {
            double tmp;
            wf.read((char*)&tmp, sizeof(double));
            values_[i] = static_cast<T>(tmp);
        }
        wf.close();
        if (!wf.good()) {
            throw eckit::SeriousBug("Error occurred at writing time!", Here());
        }
#endif
        return;
    };

    void dump(const std::string& partialPath, bool noThrow) const override  {
#if 0
        std::ostringstream os;
        os << partialPath << "-minimum-data.bin";
        std::string fname = os.str();
        std::ofstream wf(fname, std::ios::binary);
        if (!wf) {
            if (noThrow) {
                LOG_DEBUG_LIB(LibMultio) << "Cannot open dump file: fname" << std::endl;
            }
            else {
                throw eckit::SeriousBug("Cannot open file!", Here());
            }
        }
        long sz = values_.size();
        // wf.write((char *) &count_, sizeof(long));
        wf.write((char*)&sz, sizeof(long));
        for (int i = 0; i < sz; ++i) {
            double tmp = double(values_[i]);
            wf.write((char*)&tmp, sizeof(double));
        }
        wf.close();
        if (!wf.good()) {
            if (noThrow) {
                LOG_DEBUG_LIB(LibMultio) << "Error occurred at writing time: fname" << std::endl;
            }
            else {
                throw eckit::SeriousBug("Error occurred at writing time!", Here());
            }
        }
#endif
    }


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

    Maximum(const std::string& name, long sz, const std::string& partialPath, const StatisticsOptions& options) :
        Operation<T>{name, "maximum", sz, options} {
#if 0
        std::ostringstream os;
        os << partialPath << "-maximum-data.bin";
        std::string fname = os.str();
        std::ifstream wf(fname, std::ios::binary);
        if (!wf) {
            throw eckit::SeriousBug("Cannot open file!", Here());
        }
        long dim;
        wf.read((char*)&dim, sizeof(long));
        values_.resize(dim);
        for (int i = 0; i < dim; ++i) {
            double tmp;
            wf.read((char*)&tmp, sizeof(double));
            values_[i] = static_cast<T>(tmp);
        }
        wf.close();
        if (!wf.good()) {
            throw eckit::SeriousBug("Error occurred at writing time!", Here());
        }
#endif
        return;
    };

    void dump(const std::string& partialPath, bool noThrow) const override  {
#if 0
        std::ostringstream os;
        os << partialPath << "-maximum-data.bin";
        std::string fname = os.str();
        std::ofstream wf(fname, std::ios::binary);
        if (!wf) {
            if (noThrow) {
                LOG_DEBUG_LIB(LibMultio) << "Cannot open dump file: fname" << std::endl;
            }
            else {
                throw eckit::SeriousBug("Cannot open file!", Here());
            }
        }
        long sz = values_.size();
        wf.write((char*)&sz, sizeof(long));
        for (int i = 0; i < sz; ++i) {
            double tmp = double(values_[i]);
            wf.write((char*)&tmp, sizeof(double));
        }
        wf.close();
        if (!wf.good()) {
            if (noThrow) {
                LOG_DEBUG_LIB(LibMultio) << "Error occurred at writing time: fname" << std::endl;
            }
            else {
                throw eckit::SeriousBug("Error occurred at writing time!", Here());
            }
        }
#endif
    }


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

    Accumulate(const std::string& name, long sz, const std::string& partialPath, const StatisticsOptions& options) :
        Operation<T>{name, "accumulate", sz, options} {
        std::ostringstream os;
        os << partialPath << "-accumulate-data.bin";
        std::string fname = os.str();
        std::ifstream wf(fname, std::ios::binary);
        if (!wf) {
            throw eckit::SeriousBug("Cannot open file!", Here());
        }
        long dim;
        wf.read((char*)&dim, sizeof(long));
        if (dim != sz / sizeof(T)) {
            throw eckit::SeriousBug("Wrong size during restart of accumulate-statistics", Here());
        }
        values_.resize(dim);
        for (int i = 0; i < dim; ++i) {
            double tmp;
            wf.read((char*)&tmp, sizeof(double));
            values_[i] = static_cast<T>(tmp);
        }
        wf.close();
        if (!wf.good()) {
            throw eckit::SeriousBug("Error occurred at writing time!", Here());
        }
        return;
    };

    void dump(const std::string& partialPath, bool noThrow) const override  {
        std::ostringstream os;
        os << partialPath << "-accumulate-data.bin";
        std::string fname = os.str();
        std::ofstream wf(fname, std::ios::binary);
        if (!wf) {
            if (noThrow) {
                LOG_DEBUG_LIB(LibMultio) << "Cannot open dump file: fname" << std::endl;
            }
            else {
                throw eckit::SeriousBug("Cannot open file!", Here());
            }
        }
        long sz = values_.size();
        wf.write((char*)&sz, sizeof(long));
        for (int i = 0; i < sz; ++i) {
            double tmp = double(values_[i]);
            wf.write((char*)&tmp, sizeof(double));
        }
        wf.close();
        if (!wf.good()) {
            if (noThrow) {
                LOG_DEBUG_LIB(LibMultio) << "Error occurred at writing time: fname" << std::endl;
            }
            else {
                throw eckit::SeriousBug("Error occurred at writing time!", Here());
            }
        }
    }


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
std::unique_ptr<Operation<T>> make_operation(const std::string& opname, long sz, const std::string& partialPath,
                                             const StatisticsOptions& options, bool restart) {

    if (opname == "instant") {
        return restart ? std::make_unique<Instant<T>>(opname, sz, partialPath, options)
                       : std::make_unique<Instant<T>>(opname, sz, options);
    }
    if (opname == "average") {
        return restart ? std::make_unique<Average<T>>(opname, sz, partialPath, options)
                       : std::make_unique<Average<T>>(opname, sz, options);
    }
    if (opname == "flux-average") {
        return restart ? std::make_unique<FluxAverage<T>>(opname, sz, partialPath, options)
                       : std::make_unique<FluxAverage<T>>(opname, sz, options);
    }
    if (opname == "minimum") {
        return restart ? std::make_unique<Minimum<T>>(opname, sz, partialPath, options)
                       : std::make_unique<Minimum<T>>(opname, sz, options);
    }
    if (opname == "maximum") {
        return restart ? std::make_unique<Maximum<T>>(opname, sz, partialPath, options)
                       : std::make_unique<Maximum<T>>(opname, sz, options);
    }
    if (opname != "accumulate") {
        std::ostringstream os;
        os << "Invalid opname in statistics operation :: " << opname << std::endl;
        throw eckit::UserError(os.str(), Here());
    }
    return restart ? std::make_unique<Accumulate<T>>(opname, sz, partialPath, options)
                   : std::make_unique<Accumulate<T>>(opname, sz, options);
}

}  // namespace action
}  // namespace multio
