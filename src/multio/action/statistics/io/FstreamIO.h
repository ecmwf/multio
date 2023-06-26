#pragma once

#include <cinttypes>
#include <vector>

#include "multio/action/statistics/StatisticsIO.h"

namespace multio::action {

class FstreamIO final : public StatisticsIO {
public:
    FstreamIO(const std::string& path, const std::string& prefix);
    void write(const std::string& name, std::size_t writeSize) override;
    void read(const std::string& name, std::size_t readSize) override;
    void flush() override;

private:
    void checkFileExist(const std::string& fname) const;
    void checkFileSize(const std::string& fname, size_t expectedSize) const;
};

}  // namespace multio::action
