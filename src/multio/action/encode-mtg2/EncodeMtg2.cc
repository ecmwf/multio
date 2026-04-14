/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "EncodeMtg2.h"

#include <iostream>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/exception/Exceptions.h"

#include "multio/action/encode-mtg2/EncodeMtg2Exception.h"
#include "multio/datamod/MarsRecord.h"
#include "multio/datamod/MiscRecord.h"
#include "multio/datamod/Parser.h"
#include "multio/mars2mars/Rules.h"
#include "multio/message/Parametrization.h"
#include "multio/util/PrecisionTag.h"

namespace multio::action::encode_mtg2 {

namespace dm = multio::datamod;

using message::Message;
using message::Peer;


EncodeMtg2::EncodeMtg2(const ComponentConfiguration& compConf) : ChainedAction{compConf}, encoder_{} {}


void EncodeMtg2::executeImpl(Message msg) {
    if (msg.tag() != Message::Tag::Field) {
        executeNext(std::move(msg));
        return;
    }

    if (msg.payload().size() == 0) {
        throw EncodeMtg2Exception("Message has empty payload - no values to encode", Here());
    }

    auto& md = msg.metadata();

    // Read MARS keys from metadata
    auto marsRec = dm::readMetadata<dm::MarsRecord>(md);

    // Read misc keys from metadata (keys are stored with "misc-" prefix)
    auto miscRec = dm::readMetadata<dm::MiscRecord>(md);

    // Apply mars2mars mappings
    auto mappingResult = mars2mars::applyMappings(mars2mars::allRules(), marsRec, miscRec);

    // Convert records to eckit::LocalConfiguration for metkit encoder
    const auto mars = dm::writeConfig(marsRec);
    const auto misc = dm::writeConfig(miscRec, "misc-");

    executeNext(dispatchPrecisionTag(msg.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        msg.payload().acquire();
        auto values = static_cast<Precision*>(msg.payload().modifyData());
        size_t size = msg.payload().size() / sizeof(Precision);

        // Check if values need scaling
        if (mappingResult && mappingResult->valuesScaleFactor) {
            ASSERT(values);

            const auto scaleFactor = *(mappingResult->valuesScaleFactor);

            if (miscRec.missingValue.has_value()) {
                const double missing = *miscRec.missingValue;
                std::transform(values, values + size, values, [&](const Precision& value) -> Precision {
                    return static_cast<Precision>(value == missing ? missing : value * scaleFactor);
                });
            }
            else {
                std::transform(values, values + size, values, [&](const Precision& value) -> Precision {
                    return static_cast<Precision>(value * scaleFactor);
                });
            }
        }

        // Call the GRIB2 encoder in metkit
        auto encoded = encoder_.encode(values, size, mars, misc);

        eckit::Buffer buf{encoded->messageSize()};
        encoded->copyInto(reinterpret_cast<uint8_t*>(buf.data()), buf.size());

        // Write mapped MARS metadata to output message
        return Message{Message::Header{Message::Tag::Field, Peer{msg.source()}, Peer{msg.destination()},
                                       dm::writeMetadata(marsRec)},
                       std::move(buf)};
    }));
}

void EncodeMtg2::print(std::ostream& os) const {
    os << "EncodeMtg2";
}

static ActionBuilder<EncodeMtg2> EncodeMtg2Builder("encode-mtg2");

}  // namespace multio::action::encode_mtg2
