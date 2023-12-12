
#pragma once

#include <cinttypes>
#include <vector>

#include "eckit/config/LocalConfiguration.h"

#include "multio/action/statistics/StatisticsIO.h"

namespace multio::action {

static eckit::LocalConfiguration no_compression = [] {
    eckit::LocalConfiguration c;
    c.set("compression", "none");
    return c;
}();


class AtlasIO final : public StatisticsIO {
public:
    AtlasIO(const std::string& path, const std::string& prefix);
    void write(const std::string& name, std::size_t writeSize) override;
    void read(const std::string& name, std::size_t writeSize) override;
    void flush() override;

private:
    void checkFileExist(const std::string& fname) const;
    void checkFileSize(const std::string& fname, size_t expectedSize) const;
};

}  // namespace multio::action
