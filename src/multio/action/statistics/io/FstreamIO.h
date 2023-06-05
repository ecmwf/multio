#pragma once

#include <array>
#include <cinttypes>

#include "multio/action/statistics/StatisticsIO.h"

namespace multio::action {

class FstreamIO : public StatisticsIO {
public:
    FstreamIO(const std::string& path, const std::string& prefix, long step);
    void writePeriod(const std::string& name, const std::array<std::uint64_t, 15>& data);
    void readPeriod(const std::string& name, std::array<std::uint64_t, 15>& data);
    void writeOperation(const std::string& name, const std::vector<double>& data);
    void readOperation(const std::string& name, std::vector<double>& data);
    void flush();

private:
    const std::string generateFileName(const std::string& name, long step_offset) const;
    void checkFileExist(const std::string& fname) const;
    void checkFileSize(const std::string& fname, size_t expectedSize) const;
    void removeOldFile(const std::string& name, long step_offset) const;
};

}  // namespace multio::action