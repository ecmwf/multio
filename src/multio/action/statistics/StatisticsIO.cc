#include "StatisticsIO.h"

#include <mutex>

#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"

#include "multio/LibMultio.h"

namespace multio::action {


uint64_t computeChecksum(const std::vector<std::uint64_t>& state) {
    uint64_t checksum = 1979339339;  // Just a big prime number
    auto last = state.cend();
    // TODO: maybe some better strategy can be used?
    std::for_each(state.cbegin(), --last, [&checksum](const int64_t& v) {
        checksum <<= 1;
        checksum *= 31;
        checksum ^= v;
    });
    return checksum;
}

StatisticsIO::StatisticsIO(const std::string& path, const std::string& prefix) :
    path_{path}, prefix_{prefix}, key_{""}, step_{0}, name_{""} {};

void StatisticsIO::setKey(const std::string& key) {
    key_ = key;
    return;
};

void StatisticsIO::setStep(long step) {
    step_ = step;
    return;
};

void StatisticsIO::setSuffix(const std::string& suffix) {
    suffix_ = suffix;
    return;
};

void StatisticsIO::reset() {
    step_ = 0;
    key_ = "";
    suffix_ = "";
    name_ = "";
    return;
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
