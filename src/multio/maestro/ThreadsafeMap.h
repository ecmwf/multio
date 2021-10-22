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
        std::lock_guard<std::mutex> locker{mutex_};
        return map_.emplace(std::forward<Args>(args)...);
    }
    template<class P>
    std::pair<typename std::unordered_map<Key, Value>::iterator,bool> insert(P&& val) {
        std::lock_guard<std::mutex> locker{mutex_};
        return map_.insert(std::forward<P>(val));
    }
    void get(const Key& key, Value& val) {
        std::lock_guard<std::mutex> locker{mutex_};
        std::swap(val, map_.at(key));
    }
    size_t erase(const Key& key) {
        std::lock_guard<std::mutex> locker{mutex_};
        return map_.erase(key);
    }
private:
    std::mutex mutex_;
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
    std::pair<typename std::unordered_map<std::string, multio::MaestroCdo>::iterator,bool> insert(std::pair<std::string,multio::MaestroCdo>&& val) {
        return map_.insert(std::forward<std::pair<std::string,multio::MaestroCdo>>(val));
    }
    multio::MaestroCdo get(const std::string& cdo_name) {
        multio::MaestroCdo c;
        map_.get(cdo_name, c);
        return c;
    }
    size_t erase(const std::string& cdo_name) {
        return map_.erase(cdo_name);
    }
private:
    ThreadsafeMap<std::string,multio::MaestroCdo> map_;
};

#endif  // multio_ThreadsafeMap_H
