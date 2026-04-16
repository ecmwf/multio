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

#include "eckit/exception/Exceptions.h"

#include "multio/action/ChainedAction.h"

#include "multio/datamod/core/EntryDef.h"
#include "multio/datamod/MarsKeys.h"
#include "multio/datamod/GribKeys.h"

namespace multio::action::average_rate {

namespace {
using Param2ParamMap = std::unordered_map<std::int64_t, std::int64_t>;
}

namespace dm = multio::datamod;

//---------------------------- Input Metadata Keys ----------------------------

struct AverageRateKeys {
    dm::EntryType_t<decltype(dm::PARAM)> param;
    dm::EntryType_t<decltype(dm::TIMESPAN)> timespan;
    dm::EntryType_t<decltype(dm::STATTYPE)> stattype;
    dm::EntryType_t<decltype(dm::BitmapPresent)> bitmapPresent;
    dm::EntryType_t<decltype(dm::MissingValue)> missingValue;

    static constexpr std::string_view record_name_ = "average-rate";
    static constexpr auto record_entries_ = std::make_tuple(
        dm::PARAM,                   // access: read/write
        dm::TIMESPAN.tagRequired(),  // access: read only
        dm::STATTYPE,                // access: read only (must be unset)
        dm::BitmapPresent,           // access: read only
        dm::MissingValue             // access: read/write (will be unset if bitmapPresent false)
    );

    static void applyDefaults(AverageRateKeys& k) {
        if (!k.bitmapPresent.get()) {
            k.missingValue.unset();
        }
    }

    static void validate(const AverageRateKeys& k) {
        if (k.timespan.get().toSeconds() == 0) {
            throw eckit::SeriousBug(
                "The average-rate action cannot handle messages with timespan set to zero!",
                Here()
            );
        }

        if (k.stattype.isSet()) {
            throw eckit::SeriousBug(
                "The average-rate action cannot handle messages with stattype set!",
                Here()
            );
        }

        if (k.bitmapPresent.get() && !k.missingValue.isSet()) {
            throw eckit::SeriousBug(
                "Value for missingValue is required if bitmapPresent is true!",
                Here()
            );
        }
    }
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
