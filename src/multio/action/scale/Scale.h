#pragma once

#include "multio/action/ChainedAction.h"
#include "multio/datamod/core/EntryDef.h"
#include "multio/datamod/MarsKeys.h"
#include "multio/datamod/GribKeys.h"

namespace multio::action::scale {

namespace dm = multio::datamod;

//-------------------------- Action Metadata Keys ---------------------------//

struct ScaleMetadataKeys {
    dm::EntryType_t<decltype(dm::PARAM)> param;
    dm::EntryType_t<decltype(dm::MissingValue)> missingValue;

    static constexpr std::string_view record_name_ = "scale-action-metadata";
    static constexpr auto record_entries_ = std::make_tuple(
        dm::PARAM,        // access: read/write
        dm::MissingValue  // access: read only
    );
};

//------------------------ Action Configuration Keys ------------------------//
//
// TODO: Support (lists of) sub-records in datamod
//
// struct ScaleMappingKeys {
//     dm::Entry<std::int64_t> paramIn;
//     dm::Entry<std::int64_t> paramOut;
//     dm::Entry<double> scaling;
//
//     static constexpr std::string_view record_name_ = "scale-mapping-keys";
//
//     using SMK = ScaleMappingKeys;
//     static constexpr auto record_entries_ = std::make_tuple(
//         dm::entryDef("param-in", &SMK::paramIn),
//         dm::entryDef("param-out", &SMK::paramOut),
//         dm::entryDef("scaling", &SMK::scaling)
//     );
// };
//
// struct ScaleConfigurationKeys {
//     dm::Entry<std::string> presetMappings;
//     dm::Entry<std::List<ScaleMappingKeys>> customMappings;
//
//     static constexpr std::string_view record_name_ = "scale-action-configuration";
//
//     using SCK = ScaleConfigurationKeys;
//     static constexpr auto record_entries_ = std::make_tuple(
//         dm::entryDef("preset-mappings", &SCK::presetMappings).tagOptional(),
//         dm::entryDef("custom-mappings", &SCK::customMappings).tagOptional()
//     );
// };
//
//---------------------------------------------------------------------------//

using Param = std::int64_t;

struct MappingResult {
    Param paramOut;
    double scaling;
};

using Mappings = std::unordered_map<Param, MappingResult>;

class Scale final : public ChainedAction {
public:
    explicit Scale(const ComponentConfiguration& compConf);  // Constructor declaration

    void executeImpl(message::Message msg) override;

private:
    void print(std::ostream&) const override;

    const Mappings mappings_;
};

}  // namespace multio::action::scale
