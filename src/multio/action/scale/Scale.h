#pragma once

#include <cstdint>
#include <optional>

#include "eckit/exception/Exceptions.h"

#include "multio/action/ChainedAction.h"
#include "multio/datamod/Parser.h"
#include "multio/datamod/types/Param.h"
#include "multio/util/config/Parser.h"

namespace multio::action::scale {

namespace dm = multio::datamod;
namespace cf = multio::util::config;

//-------------------------- Action Metadata Keys ---------------------------//

struct ScaleMetadata {
    dm::Param param;
    std::optional<bool> bitmapPresent;
    std::optional<double> missingValue;

    static constexpr auto fields_
        = std::make_tuple(dm::requiredEntry("param", &ScaleMetadata::param),
                          dm::optionalEntry("misc-bitmapPresent", &ScaleMetadata::bitmapPresent),
                          dm::optionalEntry("misc-missingValue", &ScaleMetadata::missingValue));

    /// Apply defaults: unset missingValue if bitmapPresent is false or unset
    void applyDefaults() {
        if (!bitmapPresent.value_or(false)) {
            missingValue.reset();
        }
    }

    /// Validate: require missingValue when bitmapPresent is true
    void validate() const {
        if (bitmapPresent.value_or(false) && !missingValue.has_value()) {
            throw eckit::SeriousBug("Value for missingValue is required if bitmapPresent is true!", Here());
        }
    }
};

//------------------------ Action Configuration Keys ------------------------//

enum class ScalePreset : std::size_t
{
    LocalToWmo,
    WmoToLocal,
};

struct ScaleMapping {
    std::int64_t paramIn;
    std::int64_t paramOut;
    double scaling;

    static constexpr auto fields_ = std::make_tuple(cf::requiredEntry("param-in", &ScaleMapping::paramIn),
                                                    cf::requiredEntry("param-out", &ScaleMapping::paramOut),
                                                    cf::requiredEntry("scaling", &ScaleMapping::scaling));
};

struct ScaleConfig {
    std::optional<ScalePreset> preset;
    std::vector<ScaleMapping> customMappings;

    static constexpr auto fields_ = std::make_tuple(cf::optionalEntry("preset-mappings", &ScaleConfig::preset),
                                                    cf::optionalEntry("custom-mappings", &ScaleConfig::customMappings));
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
