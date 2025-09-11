/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "AverageRate.h"

#include "eckit/config/Configuration.h"

#include "multio/LibMultio.h"
#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/Glossary.h"
#include "multio/datamod/MarsMiscGeo.h"

namespace multio::action::average_rate {

namespace dm = multio::datamod;

namespace {
Param2ParamMap getMappings() {
    eckit::LocalConfiguration mappingConf{eckit::YAMLConfiguration{eckit::PathName{
        multio::LibMultio::instance().libraryHome() + "/share/multio/mappings/average_rate_param_mappings.yaml"
    }}};

    Param2ParamMap paramMappings;
    for (auto& mappings : mappingConf.getSubConfigurations()) {
        const auto paramIn = mappings.getInt64("paramIn");
        const auto paramOut = mappings.getInt64("paramOut");
        paramMappings[paramIn] = paramOut;
    }
    return paramMappings;
}
}

AverageRate::AverageRate(const ComponentConfiguration& compConf) :
    ChainedAction(compConf), paramMappings_{getMappings()} {}

void AverageRate::executeImpl(message::Message msg) {
    // Skip non-field messages
    if (msg.tag() != message::Message::Tag::Field) {
        executeNext(std::move(msg));
        return;
    }

    const auto md = msg.metadata();

    // Throw exception on non-statistics messages
    if (!dm::parseEntry(dm::TIMESPAN, md).isSet()) {
        std::ostringstream os;
        os << "Action average-rate cannot process non-statistics messages : " << msg;
        throw eckit::UserError(os.str(), Here());
    }

    // Throw exception on multi-loop statistics messages
    if (dm::parseEntry(dm::STATTYPE, md).isSet()) {
        std::ostringstream os;
        os << "Action average-rate cannot process multi-loop statistics messages : " << msg;
        throw eckit::UserError(os.str(), Here());
    }

    // Find and apply the mapping for this param, throw exception if not available
    const auto param = md.get<std::int64_t>(dm::legacy::Param);
    if (auto search = paramMappings_.find(param); search != paramMappings_.end()) {
        msg.acquire();
        msg.modifyMetadata().set(dm::legacy::Param, search->second);
    } else {
        std::ostringstream os;
        os << "Action average-rate cannot find mapping from param " << param << " : " << msg;
        throw eckit::UserError(os.str(), Here());
    }

    // Apply the computation on the values
    util::dispatchPrecisionTag(msg.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        compute<Precision>(msg);
    });

    executeNext(std::move(msg));
}

template <typename Precision>
void AverageRate::compute(message::Message& msg) const {
    const size_t size = msg.payload().size() / sizeof(Precision);
    auto* data = static_cast<Precision*>(msg.payload().modifyData());
    const auto& md = msg.metadata();

    const auto timespan = dm::parseEntry(dm::TIMESPAN, md);
    ASSERT(timespan.isSet());
    const auto timespanInSeconds = timespan.get().toSeconds();
    ASSERT(timespanInSeconds > 0);
    const double c = 1.0 / timespanInSeconds;

    // Compute with/without missing value
    const auto missingValue = dm::parseEntry(dm::MissingValue, md);
    if (missingValue.isSet()) {
        const auto m = static_cast<Precision>(missingValue.get());
        std::transform(data, data + size, data,
                       [c, m](Precision v) { return static_cast<Precision>(v == m ? m : v * c); });
    } else {
        std::transform(data, data + size, data,
                       [c](Precision v) { return static_cast<Precision>(v * c); });
    }
}

template void AverageRate::compute<float>(message::Message&) const;
template void AverageRate::compute<double>(message::Message&) const;

void AverageRate::print(std::ostream& os) const {
    os << "Average Rate Action";
}

static ActionBuilder<AverageRate> AverageRateBuilder("average-rate");

}  // namespace multio::action::average_rate
