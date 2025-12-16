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

#include "multio/action/encode-mtg2/AtlasGeoSetter.h"
#include "multio/action/encode-mtg2/EncodeMtg2Exception.h"
#include "multio/datamod/MarsMiscGeo.h"
#include "multio/datamod/core/EntryDumper.h"
#include "multio/datamod/core/EntryParser.h"
#include "multio/datamod/core/Record.h"
#include "multio/mars2mars/Rules.h"
#include "multio/message/Parametrization.h"
#include "multio/util/PrecisionTag.h"

namespace multio::action::encode_mtg2 {

namespace dm = multio::datamod;

using message::Message;
using message::Peer;


EncodeMtg2::EncodeMtg2(const ComponentConfiguration& compConf) :
    ChainedAction{compConf}, opts_{cf::parseActionConfig<EncodeMtg2Options>(compConf)}, encoder_{} {
    if (opts_.cached) {
        throw eckit::NotImplemented{"Encoder cache is currently not implemented!", Here()};
    }
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

    auto scopedGeom = dm::getGeometryRecord(marsRec);

    // If grid.. check if atlas is given.
    if (marsRec.grid.isSet()) {
        std::string scope{std::visit([](const auto& k) { return dm::getRecordScope(k); }, scopedGeom)};
        const auto& global = message::Parametrization::instance().get();
        // Fetch atlas and store in global parametrization (by scoping keys...)
        // Scoping here may be refactored
        if (opts_.geoFromAtlas && (global.find(scope) == global.end())) {
            extract::AtlasGeoSetter::handleGrid(scope, marsRec.grid.get());
        }
    }

    // Read & unscope geo keys from metadata
    auto geomRec = std::visit(
        [&](auto&& scopedGeom) -> dm::Geometry {
            dm::readRecord(scopedGeom, md);
            return unscopeRecord(std::move(scopedGeom));
        },
        std::move(scopedGeom));

    // Apply mappings
    auto mappingResult = mars2mars::applyMappings(mars2mars::allRules(), marsRec, miscRec);

    const auto mars = dm::dumpRecord<eckit::LocalConfiguration>(marsRec);
    const auto misc = dm::dumpRecord<eckit::LocalConfiguration>(miscRec);
    const auto geom
        = std::visit([](const auto& geomRec) { return dm::dumpRecord<eckit::LocalConfiguration>(geomRec); }, geomRec);

    executeNext(dispatchPrecisionTag(msg.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        msg.payload().acquire();
        auto values = static_cast<Precision*>(msg.payload().modifyData());
        size_t size = msg.payload().size() / sizeof(Precision);

        // Check if values need scaling
        if (mappingResult && mappingResult->valuesScaleFactor) {
            ASSERT(values);

            const auto scaleFactor = *(mappingResult->valuesScaleFactor);
            std::transform(values, values + size, values,
                           [&](const Precision& value) -> Precision { return value * scaleFactor; });
        }

        // Call the GRIB2 encoder in metkit
        const auto sample = encoder_.encode(mars, misc, geom, values, size);

        eckit::Buffer buf{sample->messageSize()};
        sample->copyInto(reinterpret_cast<uint8_t*>(buf.data()), buf.size());

        // TODO(pgeier) write mapped metadata
        return Message{Message::Header{Message::Tag::Field, Peer{msg.source()}, Peer{msg.destination()},
                                       dm::dumpRecord<message::Metadata>(marsRec)},
                       std::move(buf)};
    }));
}

void EncodeMtg2::print(std::ostream& os) const {
    os << "EncodeMtg2{cached=" << (opts_.cached ? "true" : "false")
       << ", geo-from-atlas=" << (opts_.geoFromAtlas ? "true" : "false") << "}";
}

static ActionBuilder<EncodeMtg2> EncodeMtg2Builder("encode-mtg2");

}  // namespace multio::action::encode_mtg2
