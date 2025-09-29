#pragma once

#include "eckit/exception/Exceptions.h"

#include "multio/action/ChainedAction.h"
#include "multio/datamod/core/EntryDef.h"
#include "multio/datamod/core/NestedRecord.h"
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

enum class Preset : std::size_t
{
    LocalToWmo,  // "local-to-wmo"
    WmoToLocal   // "wmo-to-local"
};

struct ScaleMappingConfig {
    dm::Entry<std::int64_t> paramIn;
    dm::Entry<std::int64_t> paramOut;
    dm::Entry<double> scaling;

    static constexpr std::string_view record_name_ = "scale-mapping-config";

    static constexpr auto record_entries_ = std::make_tuple(
        dm::entryDef("param-in", &ScaleMappingConfig::paramIn),
        dm::entryDef("param-out", &ScaleMappingConfig::paramOut),
        dm::entryDef("scaling", &ScaleMappingConfig::scaling)
    );
};

struct ScaleConfig {
    dm::Entry<Preset> presetMappings;
    dm::NestedEntry_t<std::vector<ScaleMappingConfig>> customMappings;

    static constexpr std::string_view record_name_ = "scale-action-config";

    static constexpr auto record_entries_ = std::make_tuple(
        dm::entryDef("preset-mappings", &ScaleConfig::presetMappings).tagOptional(),

        // Custom mappings are user defined mappings that will be applied on top of a preset, if a mapping from a
        // param already exists, it will be overwritten by the custom mapping.
        dm::entryDef("custom-mappings", &ScaleConfig::customMappings).tagOptional()
    );

    static void validate(const ScaleConfig& k) {
        if (!k.presetMappings.isSet() && (!k.customMappings.isSet() || k.customMappings.get().empty())) {
            throw eckit::UserError(
                "Either 'preset-mappings' or 'custom-mappings' must be set in scale action configuration!",
                Here()
            );
        }
    }
};

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


namespace multio::util {

template <>
struct TypeToString<action::scale::ScaleMappingConfig> {
    std::string operator()() const { return std::string("ScaleMappingConfig"); };
};

template <>
struct TypeToString<action::scale::Preset> {
    std::string operator()() const { return std::string("PresetMappings"); };
};

}  // namespace multio::util


namespace multio::datamod {

template <>
struct ParseType<action::scale::Preset> {
    static action::scale::Preset parse(const std::string& s);
};

}  // namespace multio::datamod
