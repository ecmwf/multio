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

#include "eckit/config/Configuration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"


#include "multio/LibMultio.h"
#include "multio/util/PrecisionTag.h"
#include "multio/message/Message.h"


namespace multio::action {

Scale::Scale(const ComponentConfiguration& compConf) :
    ChainedAction(compConf),
    cfg_{compConf},
    scalingFactor_{compConf.parsedConfig().getDouble("scaling-Factor"),1.0} {}

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
    eckit::Buffer& buffer = msg.payload();
    
    Precision* data = reinterpret_cast<Precision *>(buffer.data());
    const size_t size = msg.payload().size()/sizeof(Precision);
    cfg_.haveMissingValue() ? computeWithMissing(data, size, scalingFactor_) : computeWithoutMissing(data, size, scalingFactor_);
        
    //TODO LOOK WHAT IS NEEDED FOR METADATA then do message::Metadata md = msg.metadata(); modify md in return pass md instead of msg.metatdata()
    return {message::Message::Header{msg.tag(), msg.source(), msg.destination(), std::move(msg.metadata())},
            std::move(buffer)};
}

void Scale::print(std::ostream & os) const {
    os << "Scale (Scaling factor = " <<  scalingFactor_ << " )" ;
}
static ActionBuilder<Scale> ScaleBuilder("scale");

private:
    template <typename T>
    void scaleWithMissing(T* data, std::size_t size, double scalingFactor) {
        const double m = cfg_.missingValue();
        std::transform(data, data + size, data, [scalingFactor, m](T value) {return static_cast<T>(m==value ? m : value * scalingFactor);});
    }

    template <typename T>
    void scaleWithoutMissing(T* data, std::size_t size, double scalingFactor) {
        // Apply the scaling in place to each element
        std::transform(data, data + size, data, [scalingFactor](T value) {
            return static_cast<T>(value * scalingFactor);
        });
    }
}