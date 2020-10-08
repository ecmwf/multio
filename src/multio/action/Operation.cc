#include "Operation.h"

#include <algorithm>
#include <cstring>
#include <functional>
#include <iostream>
#include <map>

#include "eckit/exception/Exceptions.h"

#include "multio/LibMultio.h"

namespace multio {
namespace action {

Operation::Operation(const std::string& name, long sz) :
    name_{name}, values_{std::vector<double>(sz)} {}

const std::string& Operation::name() {
    return name_;
}

std::ostream& operator<<(std::ostream& os, const Operation& a) {
    a.print(os);
    return os;
}

//===============================================================================

Instant::Instant(const std::string& name, long sz) : Operation{name, sz} {}

const std::vector<double>& Instant::compute() {
    return values_;
}

void Instant::update(const double* val, long sz) {
    ASSERT(values_.size() == static_cast<size_t>(sz));

    // May never be needed -- just creates an unnecessarily copy
    std::copy(val, val + sz, values_.begin());
    LOG_DEBUG_LIB(LibMultio) << " ======== " << *this
                             << ": minimum: " << *std::min_element(begin(values_), end(values_))
                             << ", maximum: " << *std::max_element(begin(values_), end(values_))
                             << std::endl;
}

void Instant::print(std::ostream& os) const {
    os << "Operation(instant)";
}

//===============================================================================

Average::Average(const std::string& name, long sz) : Operation{name, sz} {}

const std::vector<double>& Average::compute() {
    for(auto& val : values_) {
        val /= static_cast<double>(count_);
    }
    LOG_DEBUG_LIB(LibMultio) << " ======== " << *this
                             << ": minimum: " << *std::min_element(begin(values_), end(values_))
                             << ", maximum: " << *std::max_element(begin(values_), end(values_))
                             << ", count: " << count_ << std::endl;

    return values_;
}

void Average::update(const double* val, long sz) {
    ASSERT(values_.size() == static_cast<size_t>(sz));

    for (auto& v : values_) {
        v += *val++;
    }
    ++count_;
}

void Average::print(std::ostream& os) const {
    os << "Operation(average)";
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
    LOG_DEBUG_LIB(LibMultio) << " ======== " << *this
                             << ": minimum: " << *std::min_element(begin(values_), end(values_))
                             << ", maximum: " << *std::max_element(begin(values_), end(values_))
                             << std::endl;
}

void Minimum::print(std::ostream& os) const {
    os << "Operation(minimum)";
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
    LOG_DEBUG_LIB(LibMultio) << " ======== " << *this
                             << ": minimum: " << *std::min_element(begin(values_), end(values_))
                             << ", maximum: " << *std::max_element(begin(values_), end(values_))
                             << std::endl;
}

void Maximum::print(std::ostream& os) const {
    os << "Operation(maximum)";
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
    LOG_DEBUG_LIB(LibMultio) << " ======== " << *this
                             << ": minimum: " << *std::min_element(begin(values_), end(values_))
                             << ", maximum: " << *std::max_element(begin(values_), end(values_))
                             << std::endl;
}

void Accumulate::print(std::ostream& os) const {
    os << "Operation(accumulate)";
}

//===============================================================================

namespace {

using make_oper_type = std::function<std::unique_ptr<Operation>(const std::string&, long)>;

std::unique_ptr<Operation> make_instant(const std::string& nm, long sz) {
    return std::unique_ptr<Operation>{new Instant{nm, sz}};
}

std::unique_ptr<Operation> make_average(const std::string& nm, long sz) {
    return std::unique_ptr<Operation>{new Average{nm, sz}};
}

std::unique_ptr<Operation> make_minimum(const std::string& nm, long sz) {
    return std::unique_ptr<Operation>{new Minimum{nm, sz}};
}

std::unique_ptr<Operation> make_maximum(const std::string& nm, long sz) {
    return std::unique_ptr<Operation>{new Maximum{nm, sz}};
}

std::unique_ptr<Operation> make_accumulate(const std::string& nm, long sz) {
    return std::unique_ptr<Operation>{new Accumulate{nm, sz}};
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
