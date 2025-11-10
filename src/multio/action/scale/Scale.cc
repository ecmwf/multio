/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "multio/action/scale/Scale.h"

#include "eckit/config/LocalConfiguration.h"
#include "eckit/exception/Exceptions.h"

#include "multio/LibMultio.h"
#include "multio/datamod/ContainerInterop.h"
#include "multio/message/Message.h"
#include "multio/util/PrecisionTag.h"
#include "multio/util/config/Parser.h"


namespace multio::action::scale {

const Mappings getMappings(const ScalePreset preset) {
    // Load the mapping file
    eckit::LocalConfiguration mappingConf{eckit::YAMLConfiguration{eckit::PathName{
        multio::LibMultio::instance().libraryHome() + "/share/multio/mappings/local-to-wmo.yaml"
    }}};

    // Read the mappings and put them into the map
    // We use the same mapping file for local-to-wmo and wmo-to-local, the
    // second is just the reverse mapping of the first!
    Mappings mappings;
    for (const auto& subConfig : mappingConf.getSubConfigurations()) {
        const auto mapping = util::config::parseConfig<ScaleMapping>(subConfig);
        switch (preset) {
            case ScalePreset::LocalToWmo : {
                mappings[mapping.paramIn] = {mapping.paramOut, mapping.scaling};
            }
            case ScalePreset::WmoToLocal : {
                mappings[mapping.paramOut] = {mapping.paramIn, 1.0 / mapping.scaling};
            }
        }
    }
    return mappings;
}

Mappings getMappings(const ScaleConfig& config) {
    Mappings mappings;

    // Read the preset mapping from a mappings file
    if (config.preset) {
        mappings = getMappings(config.preset.value());
    }

    // Read any user defined mappings from the action configuration
    for (const auto& mapping : config.customMappings) {
        mappings[mapping.paramIn] = {mapping.paramOut, mapping.scaling};
    }

    if (mappings.empty()) {
        throw eckit::UserError("No scale mapping was found, set 'preset' or 'mappings' in action configuration!", Here());
    }
    return mappings;
}

Scale::Scale(const ComponentConfiguration& compConf) :
    ChainedAction(compConf), mappings_{getMappings(cf::parseActionConfig<ScaleConfig>(compConf))} {}

void Scale::executeImpl(message::Message msg) {
    // Skip non-field messages
    if (msg.tag() != message::Message::Tag::Field) {
        executeNext(std::move(msg));
        return;
    }

    auto md = dm::readRecord<ScaleMetadataKeys>(msg.metadata());

    // Try to find a mapping for the incomming field
    const auto paramIn = md.param.get();
    const auto search = mappings_.find(paramIn);

    // Skip if no mapping was found
    if (search == mappings_.end()) {
        executeNext(std::move(msg));
        return;
    }

    const auto& mapping = search->second;

    msg.acquire();  // We change the message after this line

    // Map the param
    md.param.set(mapping.paramOut);
    dm::dumpRecord(md, msg.modifyMetadata());

    // Scale the payload
    util::dispatchPrecisionTag(msg.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        auto* data = static_cast<Precision*>(msg.payload().modifyData());
        const auto size = msg.payload().size() / sizeof(Precision);
        if (md.missingValue.isSet()) {
            const double missing = md.missingValue.get();
            std::transform(data, data + size, data, [&](Precision value) {
                return static_cast<Precision>(value == missing ? missing : value * mapping.scaling);
            });
        } else {
            std::transform(data, data + size, data, [&](Precision value) {
                return static_cast<Precision>(value * mapping.scaling);
            });
        }
    });

    executeNext(std::move(msg));
}

void Scale::print(std::ostream& os) const {
    os << "Scale Action ";
}

static ActionBuilder<Scale> ScaleBuilder("scale");

}  // namespace multio::action::scale


template <>
struct multio::util::config::detail::EnumTrait<multio::action::scale::ScalePreset> {
    static constexpr std::array values{
        std::pair{multio::action::scale::ScalePreset::LocalToWmo, "local-to-wmo"},
        std::pair{multio::action::scale::ScalePreset::WmoToLocal, "wmo-to-local"},
    };
};
