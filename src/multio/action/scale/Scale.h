#pragma once

#include <cstdint>

#include "multio/action/ChainedAction.h"
#include "multio/datamod/core/EntryDef.h"
#include "multio/datamod/MarsKeys.h"
#include "multio/datamod/GribKeys.h"
#include "multio/util/config/Parser.h"

namespace multio::action::scale {

namespace dm = multio::datamod;
namespace cf = multio::util::config;

//-------------------------- Action Metadata Keys ---------------------------//

struct ScaleMetadataKeys {
    dm::EntryType_t<decltype(dm::PARAM)> param;
    dm::EntryType_t<decltype(dm::BitmapPresent)> bitmapPresent;
    dm::EntryType_t<decltype(dm::MissingValue)> missingValue;

    static constexpr std::string_view record_name_ = "scale-action-metadata";
    static constexpr auto record_entries_ = std::make_tuple(
        dm::PARAM,          // access: read/write
        dm::BitmapPresent,  // access: read only
        dm::MissingValue    // access: read/write (will be unset if bitmapPresent false)
    );

    static void applyDefaults(ScaleMetadataKeys& k) {
        if (!k.bitmapPresent.get()) {
            k.missingValue.unset();
        }
    }

    static void validate(const ScaleMetadataKeys& k) {
        if (k.bitmapPresent.get() && !k.missingValue.isSet()) {
            throw eckit::SeriousBug(
                "Value for missingValue is required if bitmapPresent is true!",
                Here()
            );
        }
    }
};

//------------------------ Action Configuration Keys ------------------------//

enum class ScalePreset : std::size_t {
    LocalToWmo,
    WmoToLocal,
};

struct ScaleMapping {
    std::int64_t paramIn;
    std::int64_t paramOut;
    double scaling;

    static constexpr auto fields_ = std::make_tuple(
        cf::requiredEntry("param-in", &ScaleMapping::paramIn),
        cf::requiredEntry("param-out", &ScaleMapping::paramOut),
        cf::requiredEntry("scaling", &ScaleMapping::scaling)
    );
};

struct ScaleConfig {
    std::optional<ScalePreset> preset;
    std::vector<ScaleMapping> customMappings;

    static constexpr auto fields_ = std::make_tuple(
        cf::optionalEntry("preset-mappings", &ScaleConfig::preset),
        cf::optionalEntry("custom-mappings", &ScaleConfig::customMappings)
    );
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
