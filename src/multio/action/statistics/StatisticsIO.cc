#include "StatisticsIO.h"

#include <iomanip>
#include <mutex>

#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/log/Log.h"

#include "multio/LibMultio.h"

namespace multio::action {


uint64_t IOBuffer::checksum() const {
    uint64_t checksum = 1979339339;
    for (int i = 0; i < size_ - 1; ++i) {
        checksum <<= 1;
        checksum *= 31;
        checksum ^= buffer_[i];
    }
    return checksum;
};

IOBuffer::IOBuffer(std::vector<uint64_t>& buffer) : buffer_{buffer}, size_{buffer_.size()}, good_{true} {};
IOBuffer::IOBuffer(std::vector<uint64_t>& buffer, size_t size) : buffer_{buffer}, size_{0}, good_{true} {
    if (size <= buffer_.size()) {
        size_ = size;
    }
    else {
        std::ostringstream os;
        os << "ERROR : size too large for buffer";
        throw eckit::SeriousBug{os.str(), Here()};
    }
    return;
};

size_t IOBuffer::size() const {
    return size_;
};

uint64_t* IOBuffer::data() {
    return buffer_.data();
};

const uint64_t* IOBuffer::data() const {
    return buffer_.data();
};

void IOBuffer::setStatus(bool stat) {
    good_ = false;
};

bool IOBuffer::good() const {
    return good_;
};

uint64_t& IOBuffer::operator[](const size_t idx) {
    if ( idx >= size_) {
        std::ostringstream os;
        os << "ERROR : idx too large";
        std::cout << os.str() << std::endl;
        throw eckit::SeriousBug{os.str(), Here()};
    };
    return buffer_[idx];
};

const uint64_t& IOBuffer::operator[](const size_t idx) const {
    if ( idx >= size_) {
        std::ostringstream os;
        os << "ERROR : idx too large";
        std::cout << os.str() << std::endl;
        throw eckit::SeriousBug{os.str(), Here()};
    };
    return buffer_[idx];
};

void IOBuffer::zero() {
    std::transform(buffer_.cbegin(), buffer_.cbegin() + size_, buffer_.begin(),
                   [](const uint64_t& v) { return static_cast<uint64_t>(0); });
};

void IOBuffer::computeChecksum() {
    buffer_[size_ - 1] = checksum();
    return;
};

void IOBuffer::checkChecksum() const {
    if (buffer_[size_ - 1] != checksum()) {
        std::ostringstream os;
        os << "ERROR : wrong Checksum";
        throw eckit::SeriousBug{os.str(), Here()};
    }
    return;
};

// -------------------------------------------------------------------------------------------------------------------

StatisticsIO::StatisticsIO(const std::string& path, const std::string& prefix, const std::string& ext) :
    path_{path}, prefix_{prefix}, prevStep_{0}, currStep_{0}, key_{""}, name_{""}, ext_{ext}, buffer_{8192, 0} {};


void StatisticsIO::setKey(const std::string& key) {
    key_ = key;
    return;
};

void StatisticsIO::setCurrStep(long step) {
    currStep_ = step;
    return;
};

void StatisticsIO::setPrevStep(long step) {
    prevStep_ = step;
    return;
};

void StatisticsIO::setSuffix(const std::string& suffix) {
    suffix_ = suffix;
    return;
};

void StatisticsIO::reset() {
    currStep_ = 0;
    prevStep_ = 0;
    key_ = "";
    suffix_ = "";
    name_ = "";
    std::transform(buffer_.cbegin(), buffer_.cend(), buffer_.begin(),
                   [](const std::uint64_t v) { return static_cast<std::uint64_t>(0.0); });
    return;
};

IOBuffer StatisticsIO::getBuffer(std::size_t size) {
    std::size_t tmp = buffer_.size();
    while (tmp < size) {
        tmp *= 2;
    }
    buffer_.resize(tmp);
    return IOBuffer{buffer_, size};
};

std::string StatisticsIO::generatePathName() const {
    std::ostringstream os;
    os << path_ << "/" << prefix_ << "/" << key_ << "/" << suffix_;
    eckit::PathName{os.str()}.mkdir();
    return os.str();
};

std::string StatisticsIO::generateCurrFileName(const std::string& name) const {
    std::ostringstream os;
    os << generatePathName() << "/" << name << "-" << std::setw(10) << std::setfill('0') << currStep_ << "." << ext_;
    return os.str();
};

std::string StatisticsIO::generatePrevFileName(const std::string& name) const {
    std::ostringstream os;
    os << generatePathName() << "/" << name << "-" << std::setw(10) << std::setfill('0') << prevStep_ << "." << ext_;
    return os.str();
};

void StatisticsIO::removeCurrFile(const std::string& name) const {
    eckit::PathName file{generateCurrFileName(name)};

    if (file.exists()) {
        file.unlink();
    }
};

void StatisticsIO::removePrevFile(const std::string& name) const {
    eckit::PathName file{generatePrevFileName(name)};

    if (file.exists()) {
        file.unlink();
    }
};

//----------------------------------------------------------------------------------------------------------------------

StatisticsIOFactory& StatisticsIOFactory::instance() {
    static StatisticsIOFactory singleton;
    return singleton;
}

void StatisticsIOFactory::enregister(const std::string& name, const StatisticsIOBuilderBase* builder) {
    std::lock_guard<std::recursive_mutex> lock{mutex_};
    ASSERT(factories_.find(name) == factories_.end());
    factories_[name] = builder;
}

void StatisticsIOFactory::deregister(const std::string& name) {
    std::lock_guard<std::recursive_mutex> lock{mutex_};
    ASSERT(factories_.find(name) != factories_.end());
    factories_.erase(name);
}

void StatisticsIOFactory::list(std::ostream& out) {
    std::lock_guard<std::recursive_mutex> lock{mutex_};

    const char* sep = "";
    for (auto const& sinkFactory : factories_) {
        out << sep << sinkFactory.first;
        sep = ", ";
    }
}

std::shared_ptr<StatisticsIO> StatisticsIOFactory::build(const std::string& name, const std::string& path,
                                                         const std::string& prefix) {
    std::lock_guard<std::recursive_mutex> lock{mutex_};

    LOG_DEBUG_LIB(LibMultio) << "Looking for StatisticsIOFactory [" << name << "]" << std::endl;

    auto f = factories_.find(name);

    if (f != factories_.end())
        return f->second->make(path, prefix);

    LOG_DEBUG_LIB(LibMultio) << "No StatisticsIOFactory for [" << name << "]" << std::endl;
    LOG_DEBUG_LIB(LibMultio) << "StatisticsIOFactories are:" << std::endl;
    for (auto const& factory : factories_) {
        LOG_DEBUG_LIB(LibMultio) << "   " << factory.first << std::endl;
    }
    throw eckit::SeriousBug(std::string("No StatisticsIOFactory called ") + name);
}


StatisticsIOBuilderBase::StatisticsIOBuilderBase(const std::string& name) : name_(name) {
    StatisticsIOFactory::instance().enregister(name, this);
}

StatisticsIOBuilderBase::~StatisticsIOBuilderBase() {
    StatisticsIOFactory::instance().deregister(name_);
}

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio::action
