
#include "MaestroCdo.h"

#include <iostream>
#include <cstring>
#include "unistd.h"

namespace multio {

MaestroCdo::MaestroCdo(std::string name) : name_{name}, size_{0}, data_{nullptr} {
    declare();
}

MaestroCdo::MaestroCdo(std::string name, const void* blob, uint64_t size) : name_{name}, size_{size} {
    ASSERT(posix_memalign((void**)&data_, (size_t)sysconf(_SC_PAGESIZE), size_) == 0);
    ::memcpy(data_, blob, size_);
    declare();
    set_size_and_data();
}

MaestroCdo::~MaestroCdo() {
    dispose();
}

void MaestroCdo::declare() {
    ASSERT(MSTRO_OK == mstro_cdo_declare(name_.c_str(), MSTRO_ATTR_DEFAULT, &cdo_));
}

void MaestroCdo::seal() {
    ASSERT(MSTRO_OK == mstro_cdo_seal(cdo_));
}

void MaestroCdo::offer() {
    ASSERT(MSTRO_OK == mstro_cdo_offer(cdo_));
}

void MaestroCdo::require() {
    ASSERT(MSTRO_OK == mstro_cdo_require(cdo_));
}

void MaestroCdo::withdraw() {
    ASSERT(MSTRO_OK == mstro_cdo_withdraw(cdo_));
}

void MaestroCdo::demand() {
    ASSERT(MSTRO_OK == mstro_cdo_demand(cdo_));
    get_size_and_data();
}

void MaestroCdo::retract() {
    ASSERT(MSTRO_OK == mstro_cdo_retract(cdo_));
}

void MaestroCdo::dispose() {
    ASSERT(MSTRO_OK == mstro_cdo_dispose(cdo_));
}

void MaestroCdo::set_size_and_data() {
    ASSERT(MSTRO_OK == mstro_cdo_attribute_set(cdo_, ".maestro.core.cdo.raw-ptr", data_, false));
    ASSERT(MSTRO_OK == mstro_cdo_attribute_set(cdo_, ".maestro.core.cdo.scope.local-size", &size_, true));
}

void MaestroCdo::get_size_and_data() {
    mstro_status s = mstro_cdo_access_ptr(cdo_, &data_, &size_);
    if (s != MSTRO_OK)
        std::cout << "[i] The demnaded CDO does not contain any data." << std::endl;
}

void MaestroCdo::print(std::ostream& os) const {
    os << "Cdo name: " << name_;
}

}  // namespace multio
