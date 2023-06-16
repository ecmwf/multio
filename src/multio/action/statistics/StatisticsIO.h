#pragma once

#include <algorithm>
#include <cinttypes>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>

#include "eckit/filesystem/PathName.h"

namespace multio::action {

uint64_t computeChecksum(const std::vector<std::uint64_t>& state);

class StatisticsIO {
public:
    StatisticsIO(const std::string& path, const std::string& prefix, const std::string& ext);
    virtual ~StatisticsIO() = default;

    void setKey(const std::string& key);
    void setStep(long step);
    void setSuffix(const std::string& suffix);
    void reset();

    virtual void write(const std::string& name, const std::vector<std::uint64_t>& data) = 0;
    virtual void read(const std::string& name, std::vector<std::uint64_t>& data) = 0;
    virtual void flush() = 0;

protected:
    std::string generatePathName() const;
    std::string generateFileName(const std::string& name, long step_offset) const;
    void removeOldFile(const std::string& name, long step_offset) const;

    const std::string path_;
    const std::string prefix_;
    long step_;

    std::string key_;
    std::string suffix_;
    std::string name_;
    const std::string ext_;
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
