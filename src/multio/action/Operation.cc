#include "Operation.h"

#include <cstring>
#include <functional>
#include <map>

#include "eckit/exception/Exceptions.h"

namespace multio {
namespace action {

Operation::Operation(long sz) : values_{std::vector<double>(sz)} {}

//===============================================================================

// May never be needed -- just creates an unnecessarily copy

Instant::Instant(long sz) : Operation{sz} {}

const std::vector<double>& Instant::compute() {
    return values_;
}

void Instant::update(const double* val, long sz) {
    ASSERT(values_.size() == static_cast<size_t>(sz));

    std::memcpy(values_.data(), val, sz);
}

//===============================================================================

Average::Average(long sz) : Operation{sz} {}

const std::vector<double>& Average::compute() {
    for(auto& val : values_) {
        val /= static_cast<double>(count_);
    }
    count_ = 0;

    return values_;
}

void Average::update(const double* val, long sz) {
    ASSERT(values_.size() == static_cast<size_t>(sz));

    for (auto& v : values_) {
        v += *val++;
    }
}

//===============================================================================

Minimum::Minimum(long sz) : Operation{sz} {}

void Minimum::update(const double* val, long sz) {
    ASSERT(values_.size() == static_cast<size_t>(sz));

    for (auto& v : values_) {
        v = (v > *val) ? *val : v;
        ++val;
    }
}

//===============================================================================

Maximum::Maximum(long sz) : Operation{sz} {}

void Maximum::update(const double* val, long sz) {
    ASSERT(values_.size() == static_cast<size_t>(sz));

    for (auto& v : values_) {
        v = (v < *val) ? *val : v;
        ++val;
    }
}

//===============================================================================

Accumulate::Accumulate(long sz) : Operation{sz} {}

void Accumulate::update(const double* val, long sz) {
    ASSERT(values_.size() == static_cast<size_t>(sz));

    for (auto& v : values_) {
        v += *val++;
    }
}

//===============================================================================

namespace {

using make_oper_type = std::function<std::unique_ptr<Operation>(long)>;

std::unique_ptr<Operation> make_instant(long sz) {
    return std::unique_ptr<Instant>{new Instant{sz}};
}

std::unique_ptr<Operation> make_average(long sz) {
    return std::unique_ptr<Average>{new Average{sz}};
}

std::unique_ptr<Operation> make_minimum(long sz) {
    return std::unique_ptr<Minimum>{new Minimum{sz}};
}

std::unique_ptr<Operation> make_maximum(long sz) {
    return std::unique_ptr<Maximum>{new Maximum{sz}};
}

std::unique_ptr<Operation> make_accumulate(long sz) {
    return std::unique_ptr<Accumulate>{new Accumulate{sz}};
}

const std::map<std::string, make_oper_type> defined_operations{
    {"instant", make_instant},
    {"average", make_average},
    {"minimum", make_minimum},
    {"maximum", make_maximum},
    {"accumulate", make_accumulate}};

}  // namespace


std::unique_ptr<Operation> make_operation(const std::string& opname, long sz) {

    if (defined_operations.find(opname) == end(defined_operations)) {
        throw eckit::SeriousBug{"Operation " + opname + " is not defined"};
    }

    return defined_operations.at(opname)(sz);
}

}  // namespace action
}  // namespace multio
