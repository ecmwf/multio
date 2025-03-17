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

#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"


#include "multio/LibMultio.h"
#include "multio/config/ComponentConfiguration.h"
#include "multio/message/Glossary.h"
#include "multio/message/Message.h"
#include "multio/util/PrecisionTag.h"

#include "multio/action/scale/Mapping.h"
#include "multio/action/scale/MetadataUtils.h"
#include "multio/action/scale/Scaling.h"


namespace multio::action {

using message::glossary;

Scale::Scale(const ComponentConfiguration& compConf) :
    ChainedAction(compConf), scaling_{compConf}, mapping_{compConf}, paramsToScale_{} {

    const auto mappings = compConf.parsedConfig().has("mapping-definition")
                            ? compConf.parsedConfig().getSubConfigurations("mapping-definition")
                            : throw eckit::SeriousBug{"Scaling information not specified in plan", Here()};

    if (!mappings.empty()) {
        for (const auto& mapping : mappings) {
            auto matcher = mapping.getSubConfiguration("case");
            paramsToScale_.insert(matcher.getString("param-is"));
        }
    }
}

void Scale::executeImpl(message::Message msg) {
    if (msg.tag() != message::Message::Tag::Field) {
        executeNext(std::move(msg));
        return;
    }

    std::string cparam = extractParam(msg.metadata());

    // Continue if no scaling definition was specified in the plan.
    if (paramsToScale_.find(cparam) == paramsToScale_.end()) {
        executeNext(std::move(msg));
        return;
    }
    // Scale the message
    util::dispatchPrecisionTag(msg.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        ScaleMessage<Precision>(msg);  // Modify msg in place
    });
    // pass on the modified message
    executeNext(std::move(msg));
}

template <typename Precision>
void Scale::ScaleMessage(message::Message& msg) const {

    LOG_DEBUG_LIB(LibMultio) << "Scale :: Metadata of the input message :: Apply Scaling " << std::endl
                             << msg.metadata() << std::endl;
    msg.acquire();

    // Potentially work with msg = scaling_.applyScaling<Precision>(std::move(msg))
    scaling_.applyScaling<Precision>(msg);

    mapping_.applyMapping(msg.modifyMetadata());
}

void Scale::print(std::ostream& os) const {
    os << "Scale Action ";
}

static ActionBuilder<Scale> ScaleBuilder("scale");


template void Scale::ScaleMessage<float>(message::Message&) const;
template void Scale::ScaleMessage<double>(message::Message&) const;


}  // namespace multio::action
