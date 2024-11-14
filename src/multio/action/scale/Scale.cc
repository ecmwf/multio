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
#include "multio/util/PrecisionTag.h"
#include "multio/message/Message.h"
#include "multio/config/ComponentConfiguration.h"



namespace multio::action {

Scale::Scale(const ComponentConfiguration& compConf) :
    ChainedAction(compConf),
    scalingFactor_{compConf.parsedConfig().getDouble("scaling-Factor",1.0)} {}

void Scale::executeImpl(message::Message msg) {
    if (msg.tag() != message::Message::Tag::Field) {
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
    LOG_DEBUG_LIB(LibMultio) << "Scale :: Metadata of the input message :: " <<std::endl
                             <<msg.metadata() <<std::endl;

    size_t size = msg.payload().size()/sizeof(Precision);

    auto data = static_cast<Precision*>(msg.payload().modifyData());
    
    scaling(data, size, scalingFactor_);
    //TODO LOOK WHAT IS NEEDED FOR METADATA then do message::Metadata md = msg.metadata(); modify md in return pass md instead of msg.metatdata()
    return {message::Message::Header{msg.tag(), msg.source(), msg.destination(), std::move(msg.metadata())},
            std::move(msg.payload())};
}

template <typename Precision>
void Scale::scaling(Precision* data, std::size_t size, double scalingFactor) const {
    // Apply the scaling in place to each element
    std::transform(data, data + size, data, [scalingFactor](Precision value) {
        return static_cast<Precision>(value * scalingFactor);
    });
}

void Scale::print(std::ostream & os) const {
    os << "Scale (Scaling factor = " <<  scalingFactor_ << " )" ;
}


static ActionBuilder<Scale> ScaleBuilder("scale");

}