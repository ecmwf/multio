#ifndef multio_ThreadsafeMap_H
#define multio_ThreadsafeMap_H

#include <mutex>
#include <unordered_map>
#include "multio/maestro/MaestroCdo.h"

template<typename Key, typename Value>
class ThreadsafeMap {
public:
    template<typename... Args>
    std::pair<typename std::unordered_map<Key,Value>::iterator,bool> emplace(Args&&... args) {
        std::lock_guard<std::recursive_mutex> locker{mutex_};
        return map_.emplace(std::forward<Args>(args)...);
    }
    const Value& at(const Key& key) const {
        std::lock_guard<std::recursive_mutex> locker{mutex_};
        return map_.at(key);
    }
    Value& at(const Key& key) {
        std::lock_guard<std::recursive_mutex> locker{mutex_};
        return map_.at(key);
    }
    size_t erase(const Key& key) {
        std::lock_guard<std::recursive_mutex> locker{mutex_};
        return map_.erase(key);
    }
private:
    std::recursive_mutex mutex_;
    std::unordered_map<Key, Value> map_;
};


class CdoMap {
public:
    CdoMap() = default;

    CdoMap(const CdoMap& rhs) = delete;
    CdoMap(CdoMap&& rhs) noexcept = delete;

    CdoMap& operator=(const CdoMap& rhs) = delete;
    CdoMap& operator=(CdoMap&& rhs) = delete;

    static CdoMap& instance() {
        static CdoMap singleton;
        return singleton;
    }

    template<typename... Args>
    std::pair<typename std::unordered_map<std::string,multio::MaestroCdo>::iterator,bool> emplace(Args&&... args) {
        return map_.emplace(std::forward<Args>(args)...);
    }
    multio::MaestroCdo& at(const std::string& cdo_name) {
        return map_.at(cdo_name);
    }
    size_t erase(const std::string& cdo_name) {
        return map_.erase(cdo_name);
    }
private:
    ThreadsafeMap<std::string,multio::MaestroCdo> map_;
};

#endif  // multio_ThreadsafeMap_H
