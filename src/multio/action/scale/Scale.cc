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
    eckit::Log::info() << paramsToScale_ << std::endl;
}

void Scale::executeImpl(message::Message msg) {
    if (msg.tag() != message::Message::Tag::Field) {
        executeNext(std::move(msg));
        return;
    }

    std::string cparam{"xxx"};
    if (auto param = msg.metadata().getOpt<std::string>(glossary().param); param) {
        cparam = *param;
    }
    if (auto paramId = msg.metadata().getOpt<std::int64_t>(glossary().paramId); paramId) {
        cparam = std::to_string(*paramId);
    }
    else {
        throw eckit::SeriousBug{"param/paramId metadata not present", Here()};
    }

    // Continue if no scaling definition was specified in the plan.
    if (paramsToScale_.find(cparam) == paramsToScale_.end()) {
        executeNext(std::move(msg));
        return;
    }

    executeNext(util::dispatchPrecisionTag(msg.precision(), [&](auto pt) -> message::Message {
        using Precision = typename decltype(pt)::type;
        return ScaleMessage<Precision>(std::move(msg));
    }));
}

template <typename Precision>
message::Message Scale::ScaleMessage(message::Message&& msg) const {

    LOG_DEBUG_LIB(LibMultio) << "Scale :: Metadata of the input message :: Apply Scaling " << std::endl
                             << msg.metadata() << std::endl;
    msg.acquire();

    // Potentially work with msg = scaling_.applyScaling<Precision>(std::move(msg))
    scaling_.applyScaling<Precision>(msg);

    mapping_.applyMapping(msg.modifyMetadata());


    return {message::Message::Header{msg.tag(), msg.source(), msg.destination(), std::move(msg.metadata())},
            std::move(msg.payload())};
}

void Scale::print(std::ostream& os) const {
    os << "Scale Action ";
}

static ActionBuilder<Scale> ScaleBuilder("scale");


template message::Message Scale::ScaleMessage<float>(message::Message&&) const;
template message::Message Scale::ScaleMessage<double>(message::Message&&) const;


}  // namespace multio::action
