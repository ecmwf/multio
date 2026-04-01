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

#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/core/EntryDumper.h"
#include "multio/datamod/core/EntryParser.h"
#include "multio/datamod/core/Record.h"

#include "multio/action/encode-mtg2/EncodeMtg2Exception.h"
#include "multio/datamod/MarsMiscGeo.h"
#include "multio/mars2mars/Rules.h"
#include "multio/message/Parametrization.h"
#include "multio/util/PrecisionTag.h"

namespace multio::action::encode_mtg2 {

namespace dm = multio::datamod;

using message::Message;
using message::Peer;


EncodeMtg2::EncodeMtg2(const ComponentConfiguration& compConf) :
    ChainedAction{compConf},
    opts_{cf::parseActionConfig<EncodeMtg2Options>(compConf)},
    encoder_{},
    cache_{opts_.cached ? std::optional<Cache>{Cache{}} : std::optional<Cache>{}} {}


template <typename T>
std::unique_ptr<metkit::codes::CodesHandle> encode(metkit::mars2grib::Mars2Grib& encoder, std::optional<Cache>& cache,
                                                   T* values, size_t size, const dm::FullMarsRecord& marsRec,
                                                   const dm::MiscRecord& miscRec) {
    const auto mars = dm::dumpRecord<eckit::LocalConfiguration>(marsRec);
    const auto misc = dm::dumpRecord<eckit::LocalConfiguration>(miscRec);

    if (!cache) {
        return encoder.encode(values, size, mars, misc);
    }

    // Select caching keys and prehash
    PrehashedMarsKeys cacheKeySet = dm::readRecord<dm::MarsCacheRecord>(marsRec);

    // Search and return if entry already exists
    if (auto search = cache->find(cacheKeySet); search != cache->end()) {
        return encoder.finaliseEncoding(search->second, std::vector<T>(values, values + size), mars, misc);
    }

    // Otherwise prepare a new entry

    // Searching for rule...
    auto it = cache->emplace(std::move(cacheKeySet), encoder.prepare(mars, misc)).first;

    return encoder.finaliseEncoding(it->second, std::vector<T>(values, values + size), mars, misc);
}


void EncodeMtg2::executeImpl(Message msg) {
    if (msg.tag() != Message::Tag::Field) {
        executeNext(std::move(msg));
        return;
    }

    if (msg.payload().size() == 0) {
        throw EncodeMtg2Exception("Message has empty payload - no values to encode", Here());
    }

    auto& md = msg.metadata();

    // Read and set unscoped mars keys
    auto marsRec = dm::readRecord<dm::FullMarsRecord>(md);

    // Read scoped misc keys
    auto scopedMiscRec = dm::scopeRecord(dm::MiscRecord{});
    dm::readRecord(scopedMiscRec, md);
    // Write unscoped misc keys
    auto miscRec = dm::unscopeRecord(std::move(scopedMiscRec));

    // Apply mappings
    auto mappingResult = mars2mars::applyMappings(mars2mars::allRules(), marsRec, miscRec);


    executeNext(dispatchPrecisionTag(msg.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        msg.payload().acquire();
        auto values = static_cast<Precision*>(msg.payload().modifyData());
        size_t size = msg.payload().size() / sizeof(Precision);

        // Check if values need scaling
        if (mappingResult && mappingResult->valuesScaleFactor) {
            ASSERT(values);

            const auto scaleFactor = *(mappingResult->valuesScaleFactor);

            if (miscRec.missingValue.isSet()) {
                const double missing = miscRec.missingValue.get();
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
        const auto sample = encode(encoder_, cache_, values, size, marsRec, miscRec);

        eckit::Buffer buf{sample->messageSize()};
        sample->copyInto(reinterpret_cast<uint8_t*>(buf.data()), buf.size());

        // TODO(pgeier) write mapped metadata
        return Message{Message::Header{Message::Tag::Field, Peer{msg.source()}, Peer{msg.destination()},
                                       dm::dumpRecord<message::Metadata>(marsRec)},
                       std::move(buf)};
    }));
}

void EncodeMtg2::print(std::ostream& os) const {
    os << "EncodeMtg2{cached=" << (opts_.cached ? "true" : "false") << "}";
}

static ActionBuilder<EncodeMtg2> EncodeMtg2Builder("encode-mtg2");

}  // namespace multio::action::encode_mtg2
