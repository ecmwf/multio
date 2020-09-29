#include "Operation.h"

#include <cstring>
#include <functional>
#include <map>

#include "eckit/exception/Exceptions.h"

namespace multio {
namespace action {

Operation::Operation(const std::string& name, long sz) :
    name_{name}, values_{std::vector<double>(sz)} {}

const std::string& Operation::name() {
    return name_;
}

//===============================================================================

// May never be needed -- just creates an unnecessarily copy

Instant::Instant(const std::string& name, long sz) : Operation{name, sz} {}

const std::vector<double>& Instant::compute() {
    return values_;
}

void Instant::update(const double* val, long sz) {
    ASSERT(values_.size() == static_cast<size_t>(sz));

    std::memcpy(values_.data(), val, sz);
}

//===============================================================================

Average::Average(const std::string& name, long sz) : Operation{name, sz} {}

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

Minimum::Minimum(const std::string& name, long sz) : Operation{name, sz} {}

const std::vector<double>& Minimum::compute() {
    return values_;
}

void Minimum::update(const double* val, long sz) {
    ASSERT(values_.size() == static_cast<size_t>(sz));

    for (auto& v : values_) {
        v = (v > *val) ? *val : v;
        ++val;
    }
}

//===============================================================================

Maximum::Maximum(const std::string& name, long sz) : Operation{name, sz} {}

const std::vector<double>& Maximum::compute() {
    return values_;
}

void Maximum::update(const double* val, long sz) {
    ASSERT(values_.size() == static_cast<size_t>(sz));

    for (auto& v : values_) {
        v = (v < *val) ? *val : v;
        ++val;
    }
}

//===============================================================================

Accumulate::Accumulate(const std::string& name, long sz) : Operation{name, sz} {}

const std::vector<double>& Accumulate::compute() {
    return values_;
}

void Accumulate::update(const double* val, long sz) {
    ASSERT(values_.size() == static_cast<size_t>(sz));

    for (auto& v : values_) {
        v += *val++;
    }
}

//===============================================================================

namespace {

using make_oper_type = std::function<std::unique_ptr<Operation>(const std::string&, long)>;

std::unique_ptr<Operation> make_instant(const std::string& nm, long sz) {
    return std::unique_ptr<Instant>{new Instant{nm, sz}};
}

std::unique_ptr<Operation> make_average(const std::string& nm, long sz) {
    return std::unique_ptr<Average>{new Average{nm, sz}};
}

std::unique_ptr<Operation> make_minimum(const std::string& nm, long sz) {
    return std::unique_ptr<Minimum>{new Minimum{nm, sz}};
}

std::unique_ptr<Operation> make_maximum(const std::string& nm, long sz) {
    return std::unique_ptr<Maximum>{new Maximum{nm, sz}};
}

std::unique_ptr<Operation> make_accumulate(const std::string& nm, long sz) {
    return std::unique_ptr<Accumulate>{new Accumulate{nm, sz}};
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

    return defined_operations.at(opname)(opname, sz);
}

}  // namespace action
}  // namespace multio
