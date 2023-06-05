#pragma once

#include <algorithm>
#include <cinttypes>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>

#include "eckit/filesystem/PathName.h"
#include "eckit/types/DateTime.h"

namespace multio::action {

class StatisticsIO {
public:
    StatisticsIO(const std::string& path, const std::string& prefix, long step);
    void setKey(const std::string& key);
    void setStep(long step);
    void setSuffix(const std::string& suffix);

    virtual void writePeriod(const std::string& name, const std::array<std::uint64_t, 15>& data) = 0;
    virtual void readPeriod(const std::string& name, std::array<std::uint64_t, 15>& data) = 0;
    virtual void writeOperation(const std::string& name, const std::vector<double>& data) = 0;
    virtual void readOperation(const std::string& name, std::vector<double>& data) = 0;
    virtual void flush() = 0;

protected:
    const std::string path_;
    const std::string prefix_;
    long step_;

    std::string key_;
    std::string suffix_;
    std::string name_;
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

    std::shared_ptr<StatisticsIO> build(const std::string& name, const std::string& path, const std::string& prefix,
                                        long step);

private:  // members
    std::map<std::string, const StatisticsIOBuilderBase*> factories_;

    std::recursive_mutex mutex_;
};

class StatisticsIOBuilderBase : private eckit::NonCopyable {
public:  // methods
    virtual std::shared_ptr<StatisticsIO> make(const std::string& path, const std::string& prefix, long step) const = 0;

protected:  // methods
    StatisticsIOBuilderBase(const std::string&);

    virtual ~StatisticsIOBuilderBase();

    std::string name_;
};

template <class T>
class StatisticsIOBuilder final : public StatisticsIOBuilderBase {
    std::shared_ptr<StatisticsIO> make(const std::string& path, const std::string& prefix, long step) const override {
        return std::make_shared<T>(path, prefix, step);
    }

public:
    StatisticsIOBuilder(const std::string& name) : StatisticsIOBuilderBase(name) {}
};

}  // namespace multio::action