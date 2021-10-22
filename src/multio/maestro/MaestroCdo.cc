
#include "MaestroCdo.h"

#include <iostream>
#include <cstring>
#include "unistd.h"

namespace multio {

MaestroCdo::MaestroCdo(MaestroStatistics& statistics) : statistics_{statistics} {}

MaestroCdo::MaestroCdo(std::string name, MaestroStatistics& statistics) :
    name_{name}, statistics_{statistics} {
//    eckit::AutoTiming timing(statistics_.timer_, statistics_.cdoConstructionTiming_);
    declare();
}

MaestroCdo::MaestroCdo(std::string name, const void* blob, uint64_t size, MaestroStatistics& statistics) :
    name_{name}, statistics_{statistics} {
//    eckit::AutoTiming timing(statistics_.timer_, statistics_.cdoConstructionTiming_);
    declare();
    if (blob)
        set_size_and_data(size, blob);
}

MaestroCdo::MaestroCdo(MaestroCdo&& rhs) : name_(std::move(rhs.name_)),
    size_{rhs.size_}, data_{rhs.data_}, cdo_{rhs.cdo_}, statistics_{rhs.statistics_} {
//        eckit::AutoTiming timing(statistics_.timer_, statistics_.cdoMoveTiming_);
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
        statistics_ = std::move(rhs.statistics_);
    }
    return *this;
}

MaestroCdo::~MaestroCdo() {
    ASSERT(not cdo_);
}

void MaestroCdo::declare() {
//    eckit::AutoTiming timing(statistics_.timer_, statistics_.cdoDeclareTiming_);
    ASSERT(not cdo_);
    ASSERT(MSTRO_OK == mstro_cdo_declare(name_.c_str(), MSTRO_ATTR_DEFAULT, &cdo_));
}

void MaestroCdo::seal() {
//    eckit::AutoTiming timing(statistics_.timer_, statistics_.cdoSealTiming_);
    ASSERT(cdo_);
    ASSERT(MSTRO_OK == mstro_cdo_seal(cdo_));
}

void MaestroCdo::offer() {
//    eckit::AutoTiming timing(statistics_.timer_, statistics_.cdoOfferTiming_);
    ASSERT(cdo_);
    ASSERT(MSTRO_OK == mstro_cdo_offer(cdo_));
}

void MaestroCdo::require() {
//    eckit::AutoTiming timing(statistics_.timer_, statistics_.cdoRequireTiming_);
    ASSERT(cdo_);
    ASSERT(MSTRO_OK == mstro_cdo_require(cdo_));
}

void MaestroCdo::withdraw() {
//    eckit::AutoTiming timing(statistics_.timer_, statistics_.cdoWithdrawTiming_);
    ASSERT(cdo_);
    ASSERT(MSTRO_OK == mstro_cdo_withdraw(cdo_));
}

void MaestroCdo::demand() {
//    eckit::AutoTiming timing(statistics_.timer_, statistics_.cdoDemandTiming_);
    ASSERT(cdo_);
    ASSERT(MSTRO_OK == mstro_cdo_demand(cdo_));
    get_size_and_data();
}

void MaestroCdo::retract() {
//    eckit::AutoTiming timing(statistics_.timer_, statistics_.cdoRetractTiming_);
    ASSERT(cdo_);
    ASSERT(MSTRO_OK == mstro_cdo_retract(cdo_));
}

void MaestroCdo::dispose() {
//    eckit::AutoTiming timing(statistics_.timer_, statistics_.cdoDisposeTiming_);
    if (cdo_) {
        ASSERT(MSTRO_OK == mstro_cdo_dispose(cdo_));
        cdo_ = nullptr;
    }
}

void MaestroCdo::set_size_and_data(uint64_t size, const void* data) {
//    eckit::AutoTiming timing(statistics_.timer_, statistics_.cdoSetSizeAndDataTiming_);
    size_ = size;
    ASSERT(cdo_);
    // Ownership of the memory is passed to maestro-core here
    ASSERT(posix_memalign((void**)&data_, (size_t)sysconf(_SC_PAGESIZE), size_) == 0);
    ::memcpy(data_, data, size_);

    ASSERT(MSTRO_OK == mstro_cdo_attribute_set(cdo_, ".maestro.core.cdo.raw-ptr", data_, false));
    ASSERT(MSTRO_OK == mstro_cdo_attribute_set(cdo_, ".maestro.core.cdo.scope.local-size", &size_, true));
}

void MaestroCdo::get_size_and_data() {
//    eckit::AutoTiming timing(statistics_.timer_, statistics_.cdoGetSizeAndDataTiming_);
    ASSERT(cdo_);
    mstro_status s = mstro_cdo_access_ptr(cdo_, &data_, reinterpret_cast<int64_t*>(&size_));
    if (s != MSTRO_OK)
        std::cout << "[i] The demnaded CDO does not contain any data." << std::endl;
}

void MaestroCdo::print(std::ostream& os) const {
    os << "Cdo name: " << name_;
}

}  // namespace multio
