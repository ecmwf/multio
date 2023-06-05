#pragma once

#include <cinttypes>
#include <vector>

#include "multio/action/statistics/StatisticsIO.h"

namespace multio::action {

class FstreamIO : public StatisticsIO {
public:
    FstreamIO(const std::string& path, const std::string& prefix);
    void write(const std::string& name, const std::vector<std::uint64_t>& data);
    void read(const std::string& name, std::vector<std::uint64_t>& data);
    void flush();

private:
    const std::string generateFileName(const std::string& name, long step_offset) const;
    void checkFileExist(const std::string& fname) const;
    void checkFileSize(const std::string& fname, size_t expectedSize) const;
    void removeOldFile(const std::string& name, long step_offset) const;
};

}  // namespace multio::actionmultio/src/multio/action/statistics/StatisticsIO.h