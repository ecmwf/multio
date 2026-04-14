/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Kevin Nobel

/// @date Aug 2025

#pragma once

#include <cstdint>
#include <optional>

#include "eckit/exception/Exceptions.h"

#include "multio/action/ChainedAction.h"
#include "multio/datamod/Parser.h"
#include "multio/datamod/types/Param.h"
#include "multio/datamod/types/StatType.h"

namespace multio::action::average_rate {

namespace {
using Param2ParamMap = std::unordered_map<std::int64_t, std::int64_t>;
}

namespace dm = multio::datamod;

//---------------------------- Input Metadata Keys ----------------------------

struct AverageRateMetadata {
    dm::Param param;
    std::int64_t timespan;  // in hours (from MARS metadata)
    std::optional<dm::StatType> stattype;
    std::optional<bool> bitmapPresent;
    std::optional<double> missingValue;

    static constexpr auto fields_
        = std::make_tuple(dm::requiredEntry("param", &AverageRateMetadata::param),
                          dm::requiredEntry("timespan", &AverageRateMetadata::timespan),
                          dm::optionalEntry("stattype", &AverageRateMetadata::stattype),
                          dm::optionalEntry("misc-bitmapPresent", &AverageRateMetadata::bitmapPresent),
                          dm::optionalEntry("misc-missingValue", &AverageRateMetadata::missingValue));

    /// Apply defaults: unset missingValue if bitmapPresent is false or unset
    void applyDefaults() {
        if (!bitmapPresent.value_or(false)) {
            missingValue.reset();
        }
    }

    void validate() const {
        if (timespan == 0) {
            throw eckit::SeriousBug("The average-rate action cannot handle messages with timespan set to zero!",
                                    Here());
        }

        if (stattype.has_value()) {
            throw eckit::SeriousBug("The average-rate action cannot handle messages with stattype set!", Here());
        }

        if (bitmapPresent.value_or(false) && !missingValue.has_value()) {
            throw eckit::SeriousBug("Value for missingValue is required if bitmapPresent is true!", Here());
        }
    }

    /// Convert timespan from hours to seconds
    std::int64_t timespanSeconds() const { return timespan * 3600; }
};

//-----------------------------------------------------------------------------

class AverageRate : public ChainedAction {
public:
    explicit AverageRate(const ComponentConfiguration& compConf);

    void executeImpl(message::Message msg) override;

private:
    template <typename Precision>
    void compute(message::Message& msg) const;

    void print(std::ostream&) const override;

    const Param2ParamMap paramMappings_;
};

}  // namespace multio::action::average_rate
