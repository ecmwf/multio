#include "NemoToGrib.h"

#include "eckit/config/LocalConfiguration.h"
#include "eckit/config/YAMLConfiguration.h"

#include "multio/util/ConfigurationPath.h"

using multio::util::configuration_path;

namespace {
std::map<NemoKey, GribData> fetch_nemo_params(const eckit::Configuration& config) {
    const auto& cfgList = config.getSubConfigurations("nemo-fields");
    std::map<std::string, GribData> nemo_map;
    for (auto const& cfg : cfgList) {
      nemo_map[cfg.getString("nemo-id")] = {cfg.getLong("param-id"),
                                            cfg.getString("grid-type"),
                                            cfg.getString("level-type")};
    }
    return nemo_map;
}
}  // namespace

NemoToGrib::NemoToGrib() :
    parameters_{
        fetch_nemo_params(eckit::YAMLConfiguration{configuration_path() + "nemo-to-grib.yaml"})} {}

const GribData& NemoToGrib::get(const NemoKey& key) const {
    return parameters_.at(key);
}
