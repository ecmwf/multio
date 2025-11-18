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

    msg.acquire();

    auto md = dm::readRecord<AverageRateKeys>(msg.metadata());

    // Find and apply the mapping for this param, throw exception if not available
    if (auto search = paramMappings_.find(md.param.get().id()); search != paramMappings_.end()) {
        md.param.set(search->second);
        dm::dumpRecord(md, msg.modifyMetadata());
    } else {
        std::ostringstream os;
        os << "Action average-rate cannot find mapping from param " << md.param.get().id() << " : " << msg;
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
    auto md = dm::readRecord<AverageRateKeys>(msg.metadata());

    const size_t size = msg.payload().size() / sizeof(Precision);
    auto* data = static_cast<Precision*>(msg.payload().modifyData());

    const double c = 1.0 / md.timespan.get().toSeconds();

    // Compute with/without missing value
    if (md.missingValue.isSet()) {
        const auto m = static_cast<Precision>(md.missingValue.get());
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
