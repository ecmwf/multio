
#pragma once

#include <algorithm>
#include <cinttypes>
#include <fstream>
#include <iostream>
#include <map>
#include <memory>
#include <mutex>
#include <string>
#include <vector>

#include "eckit/memory/NonCopyable.h"

namespace multio::action {

class IOBuffer {
private:
    std::vector<uint64_t>& buffer_;
    size_t size_;
    bool good_;

    uint64_t checksum() const;

public:
    typename std::vector<uint64_t>::iterator begin() { return buffer_.begin(); };
    typename std::vector<uint64_t>::iterator end() { return buffer_.begin() + size_; };
    typename std::vector<uint64_t>::const_iterator cbegin() const{ return buffer_.cbegin(); };
    typename std::vector<uint64_t>::const_iterator cend() const { return buffer_.cbegin() + size_; };

    IOBuffer(std::vector<uint64_t>& buffer);
    IOBuffer(std::vector<uint64_t>& buffer, size_t size);
    size_t size() const;
    uint64_t* data();
    const uint64_t* data() const;
    void setStatus(bool stat);
    bool good() const;
    uint64_t& operator[](const size_t idx);
    const uint64_t& operator[](const size_t idx) const;
    void zero();
    void computeChecksum();
    void checkChecksum() const;
};

// -------------------------------------------------------------------------------------------------------------------
class StatisticsIO {
public:
    StatisticsIO(const std::string& path, const std::string& prefix, const std::string& ext);
    virtual ~StatisticsIO() = default;

    void setKey(const std::string& key);
    void setCurrStep(long step);
    void setPrevStep(long step);
    void setSuffix(const std::string& suffix);
    void reset();
    IOBuffer getBuffer(std::size_t size);

    virtual void write(const std::string& name, size_t writeSize) = 0;
    virtual void read(const std::string& name, size_t readSize) = 0;
    virtual void flush() = 0;


protected:
    std::string generatePathName() const;
    std::string generateCurrFileName(const std::string& name) const;
    std::string generatePrevFileName(const std::string& name) const;
    void removeCurrFile(const std::string& name) const;
    void removePrevFile(const std::string& name) const;

    const std::string path_;
    const std::string prefix_;
    long prevStep_;
    long currStep_;

    std::string key_;
    std::string suffix_;
    std::string name_;
    const std::string ext_;

    std::vector<std::uint64_t> buffer_;
};

class StatisticsIOBuilderBase;

class StatisticsIOFactory : private eckit::NonCopyable {
private:  // methods
    StatisticsIOFactory() {}

public:  // methods
    static StatisticsIOFactory& instance();

    void enregister(const std::string& name, const StatisticsIOBuilderBase* builder);
    void deregister(const std::string& name);

    void list(std::ostream&);

    std::shared_ptr<StatisticsIO> build(const std::string& name, const std::string& path, const std::string& prefix);

private:  // members
    std::map<std::string, const StatisticsIOBuilderBase*> factories_;

    std::recursive_mutex mutex_;
};

class StatisticsIOBuilderBase : private eckit::NonCopyable {
public:  // methods
    virtual std::shared_ptr<StatisticsIO> make(const std::string& path, const std::string& prefix) const = 0;

protected:  // methods
    StatisticsIOBuilderBase(const std::string&);

    virtual ~StatisticsIOBuilderBase();

    std::string name_;
};

template <class T>
class StatisticsIOBuilder final : public StatisticsIOBuilderBase {
    std::shared_ptr<StatisticsIO> make(const std::string& path, const std::string& prefix) const override {
        return std::make_shared<T>(path, prefix);
    }

public:
    StatisticsIOBuilder(const std::string& name) : StatisticsIOBuilderBase(name) {}
};

}  // namespace multio::action
