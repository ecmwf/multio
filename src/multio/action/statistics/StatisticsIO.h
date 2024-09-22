
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

#include "eckit/filesystem/PathName.h"
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
    typename std::vector<uint64_t>::const_iterator cbegin() const { return buffer_.cbegin(); };
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
    StatisticsIO(const std::string& basePath, const std::string& uniqueID, const std::string& ext);
    virtual ~StatisticsIO();

    void setDateTime(const std::string& dateTime);

    std::string pushDir( const std::string& directory );
    std::string popDir( );
    std::string getCurrentDir(  ) const;

    IOBuffer getBuffer(std::size_t size);
    std::vector<eckit::PathName> getFiles();
    std::vector<eckit::PathName> getDirs();

    virtual void write(const std::string& name, std::size_t fieldSize, size_t writeSize) = 0;
    virtual void readSize(const std::string& name, size_t& readSize ) = 0;
    virtual void read(const std::string& name, size_t readSize ) = 0;
    virtual void flush ( ) = 0;

protected:
    std::string generatePathName() const;
    std::string generateCurrFileName(const std::string& name) const;

    std::vector<std::string> path_;
    std::string basePath_;
    std::string dateTime_;
    std::string uniqueID_;
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

    std::shared_ptr<StatisticsIO> build(const std::string& name, const std::string& basePath, const std::string& uniqueID);

private:  // members
    std::map<std::string, const StatisticsIOBuilderBase*> factories_;

    std::recursive_mutex mutex_;
};

class StatisticsIOBuilderBase : private eckit::NonCopyable {
public:  // methods
    virtual std::shared_ptr<StatisticsIO> make(const std::string& basePath, const std::string& uniqueID) const = 0;

protected:  // methods
    StatisticsIOBuilderBase(const std::string&);

    virtual ~StatisticsIOBuilderBase();

    std::string name_;
};

template <class T>
class StatisticsIOBuilder final : public StatisticsIOBuilderBase {
    std::shared_ptr<StatisticsIO> make(const std::string& basePath, const std::string& uniqueID) const override {
        return std::make_shared<T>(basePath, uniqueID);
    }

public:
    StatisticsIOBuilder(const std::string& name) : StatisticsIOBuilderBase(name) {}
};

}  // namespace multio::action
