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
    if (idx >= size_) {
        std::ostringstream os;
        os << "ERROR : idx too large";
        std::cout << os.str() << std::endl;
        throw eckit::SeriousBug{os.str(), Here()};
    };
    return buffer_[idx];
};

const uint64_t& IOBuffer::operator[](const size_t idx) const {
    if (idx >= size_) {
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

StatisticsIO::StatisticsIO(const std::string& basePath, const std::string& uniqueID, const std::string& ext) :
    basePath_{basePath}, uniqueID_{uniqueID}, ext_{ext}, dateTime_{""}, buffer_{8192, 0} {
    if ( ! eckit::PathName{basePath_}.exists() ) {
        std::ostringstream os;
        os << "ERROR : base path does not exist: " << basePath_;
        throw eckit::SeriousBug{os.str(), Here()};
    }
    std::string tmp = getCurrentDir();
    return;
};

StatisticsIO::~StatisticsIO(){
    buffer_.clear();
    return;
};

void StatisticsIO::setDateTime(const std::string& dateTime) {
    dateTime_ = dateTime;
    return;
};

std::string StatisticsIO::getDateTime() {
    return dateTime_;
}

std::string StatisticsIO::pushDir(const std::string& directory) {
    path_.push_back(directory);
    return getCurrentDir();
};

std::string StatisticsIO::popDir() {
    path_.pop_back();
    return getCurrentDir();
};


IOBuffer StatisticsIO::getBuffer(std::size_t size) {
    std::size_t tmp = buffer_.size();
    while (tmp < size) {
        tmp *= 2;
    }
    buffer_.resize(tmp);
    return IOBuffer{buffer_, size};
};


std::vector<eckit::PathName> StatisticsIO::getFiles( ) {
    std::string path = getCurrentDir();
    std::vector<eckit::PathName> files;
    std::vector<eckit::PathName> dirs;
    eckit::PathName{path}.children( files, dirs );
    return files;
};


std::vector<eckit::PathName> StatisticsIO::getDirs( ) {
    std::string path = getCurrentDir();
    std::vector<eckit::PathName> files;
    std::vector<eckit::PathName> dirs;
    eckit::PathName path2{path};
    path2.children( files, dirs );
    return dirs;
};

std::string StatisticsIO::getCurrentDir() const {
    std::ostringstream os;
    os << basePath_ << "/" << uniqueID_ << "/" << dateTime_;
    for ( const auto& dir : path_ ) {
        os << "/" << dir;
    }
    eckit::PathName{os.str()}.mkdir();
    return os.str();
};

std::string StatisticsIO::getRestartSymLink() const {
    std::ostringstream os;
    os << basePath_ << "/" << uniqueID_<< "/" << "latest";
    return os.str();
};

std::string StatisticsIO::generateCurrFileName(const std::string& name) const {
    std::ostringstream os;
    os << getCurrentDir() << "/" << name << "." << ext_;
    return os.str();
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

std::shared_ptr<StatisticsIO> StatisticsIOFactory::build(const std::string& name, const std::string& basePath,
                                                         const std::string& uniqueID ) {
    std::lock_guard<std::recursive_mutex> lock{mutex_};

    LOG_DEBUG_LIB(LibMultio) << "Looking for StatisticsIOFactory [" << name << "]" << std::endl;

    auto f = factories_.find(name);

    if (f != factories_.end())
        return f->second->make(basePath, uniqueID);

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
