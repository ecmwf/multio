#include "Operation.h"

#include "eckit/exception/Exceptions.h"

namespace multio {
namespace action {

Operation::Operation(long sz) : values_{std::vector<double>(sz)} {}

//===============================================================================

Average::Average(long sz) : Operation{sz} {}

void Average::update(const double* val, long sz) {
    ASSERT(values_.size() == static_cast<size_t>(sz));

    for (auto& v : values_) {
        v += *val++;
    }
}

}  // namespace action
}
