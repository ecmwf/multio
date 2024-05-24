
#include "MaestroCdo.h"

#include <cstring>
#include <iostream>
#include "unistd.h"

namespace multio {

MaestroCdo::MaestroCdo() {}

MaestroCdo::MaestroCdo(std::string name) : name_{name} {
    declare();
    int64_t size = 0;
    ASSERT(MSTRO_OK == mstro_cdo_attribute_set(cdo_, ".maestro.core.cdo.scope.local-size", (void**)&size, true, false));
}

MaestroCdo::MaestroCdo(std::string name, const void* blob, uint64_t size) : name_{name} {
    declare();
    if (blob)
        set_size_and_data(size, blob);
}

MaestroCdo::MaestroCdo(MaestroCdo&& rhs) :
    name_(std::move(rhs.name_)), size_{rhs.size_}, data_{rhs.data_}, cdo_{rhs.cdo_} {
    rhs.size_ = 0;
    rhs.data_ = nullptr;
    rhs.cdo_ = nullptr;
}

MaestroCdo& MaestroCdo::operator=(MaestroCdo&& rhs) {
    if (this != &rhs) {
        dispose();
        cdo_ = rhs.cdo_;
        rhs.cdo_ = nullptr;
        name_ = std::move(rhs.name_);
        size_ = rhs.size_;
        rhs.size_ = 0;
        data_ = rhs.data_;
        rhs.data_ = nullptr;
    }
    return *this;
}

MaestroCdo::~MaestroCdo() {
    ASSERT(not cdo_);
}

void MaestroCdo::declare() {
    ASSERT(not cdo_);
    ASSERT(MSTRO_OK == mstro_cdo_declare(name_.c_str(), MSTRO_ATTR_DEFAULT, &cdo_));
}

void MaestroCdo::seal() {
    ASSERT(cdo_);
    ASSERT(MSTRO_OK == mstro_cdo_seal(cdo_));
}

void MaestroCdo::offer() {
    ASSERT(cdo_);
    ASSERT(MSTRO_OK == mstro_cdo_offer(cdo_));
}

void MaestroCdo::require() {
    ASSERT(cdo_);
    ASSERT(MSTRO_OK == mstro_cdo_require(cdo_));
}

void MaestroCdo::withdraw() {
    ASSERT(cdo_);
    ASSERT(MSTRO_OK == mstro_cdo_withdraw(cdo_));
}

void MaestroCdo::demand() {
    ASSERT(cdo_);
    ASSERT(MSTRO_OK == mstro_cdo_demand(cdo_));
    get_size_and_data();
}

void MaestroCdo::retract() {
    ASSERT(cdo_);
    ASSERT(MSTRO_OK == mstro_cdo_retract(cdo_));
}

void MaestroCdo::dispose() {
    if (cdo_) {
        ASSERT(MSTRO_OK == mstro_cdo_dispose(cdo_));
        cdo_ = nullptr;
        free(data_);
    }
}

void MaestroCdo::set_size_and_data(uint64_t size, const void* data) {
    size_ = size;
    ASSERT(cdo_);
    // Ownership of the memory is passed to maestro-core here
    ASSERT(posix_memalign((void**)&data_, (size_t)sysconf(_SC_PAGESIZE), size_) == 0);
    ::memcpy(data_, data, size_);

    ASSERT(MSTRO_OK == mstro_cdo_attribute_set(cdo_, ".maestro.core.cdo.raw-ptr", data_, false, true));
    ASSERT(MSTRO_OK == mstro_cdo_attribute_set(cdo_, ".maestro.core.cdo.scope.local-size", &size_, true, false));
}

void MaestroCdo::get_size_and_data() {
    ASSERT(cdo_);
    mstro_status s = mstro_cdo_access_ptr(cdo_, &data_, reinterpret_cast<int64_t*>(&size_));
    if (s != MSTRO_OK)
        std::cout << "[i] The demnaded CDO does not contain any data." << std::endl;
}

void MaestroCdo::print(std::ostream& os) const {
    os << "Cdo name: " << name_;
}

}  // namespace multio
