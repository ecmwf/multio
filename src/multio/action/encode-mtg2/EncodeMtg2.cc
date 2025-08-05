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

#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"

#include "multio/LibMultio.h"
#include "multio/action/encode-mtg2/AtlasGeoSetter.h"
#include "multio/action/encode-mtg2/EncodeMtg2Exception.h"
#include "multio/config/PathConfiguration.h"
#include "multio/datamod/Glossary.h"
#include "multio/datamod/MarsMiscGeo.h"
#include "multio/mars2grib/EncoderCache.h"
#include "multio/mars2grib/Options.h"
#include "multio/mars2mars/Rules.h"
#include "multio/message/Parametrization.h"
#include "multio/util/MioGribHandle.h"
#include "multio/util/PrecisionTag.h"

namespace multio::action {

using message::Message;
using message::Peer;


EncodeMtg2::EncodeMtg2(const ComponentConfiguration& compConf) :
    ChainedAction{compConf},
    conf_{datamod::readByValue(mars2grib::EncodeMtg2KeySet{}, compConf.parsedConfig())},
    cache_{conf_} {}


void EncodeMtg2::executeImpl(Message msg) {
    if (msg.tag() != Message::Tag::Field) {
        executeNext(std::move(msg));
        return;
    }

    auto& md = msg.metadata();

    {
        using namespace datamod;
        // Read and set unscoped mars keys
        auto marsKeys = read(keySet<MarsKeys>().unscoped(), md);

        // Read scoped misc keys
        auto miscKeys = read(keySet<MiscKeys>().scoped(), md);
        // Write unscoped misc keys
        miscKeys.keySet.unscoped();

        auto geoKeySets = getGeometryKeySet(marsKeys);

        // If grid.. check if atlas is given.
        const auto& grid = key<MarsKeys::GRID>(marsKeys);
        if (!grid.isMissing()) {
            auto optScope = std::visit([](const auto& k) { return k.getScope(); }, geoKeySets);
            if (optScope) {
                std::string scope{*optScope};
                const auto& global = message::Parametrization::instance().get();
                const auto& geoFromAtlas = key<EncodeMtg2Def::GeoFromAtlas>(conf_);
                // Fetch atlas and store in global parametrization (by scoping keys...)
                // Scoping here may be refactored
                if (geoFromAtlas.get() && (global.find(scope) == global.end())) {
                    extract::AtlasGeoSetter::handleGrid(scope, grid.get());
                }
            }
        }

        // Read & unscope geo keys from metadata
        auto geoKeys = std::visit(
            [&](auto& geoKeySet) -> Geometry {
                auto geoKeys = read(geoKeySet, md);
                geoKeys.keySet.unscoped();
                return geoKeys;
            },
            geoKeySets);


        // Apply mappings
        auto mappingResult = mars2mars::applyMappings(marsKeys, miscKeys);

        // TODO use upcoming C++ interface
        std::unique_ptr<util::MioGribHandle> sample = cache_.getHandle(marsKeys, miscKeys, geoKeys);

        if (msg.payload().size() == 0) {
            throw EncodeMtg2Exception("Message has empty payload - no values to encode", Here());
        }

        executeNext(dispatchPrecisionTag(msg.precision(), [&](auto pt) {
            using Precision = typename decltype(pt)::type;
            size_t size = msg.payload().size() / sizeof(Precision);

            // Check if values need scaling
            if (mappingResult && mappingResult->valuesScaleFactor) {
                msg.payload().acquire();
                auto values = static_cast<Precision*>(msg.payload().modifyData());
                ASSERT(values);

                const auto scaleFactor = *(mappingResult->valuesScaleFactor);
                std::transform(values, values + size, values,
                               [&](const Precision& value) -> Precision { return value * scaleFactor; });

                sample->setDataValues(values, size);
            }
            else {
                // No scaling
                auto values = static_cast<const Precision*>(msg.payload().data());
                sample->setDataValues(values, size);
            }

            eckit::Buffer buf{sample->length()};
            sample->write(buf);

            // TODO write mapped metadata
            return Message{Message::Header{Message::Tag::Field, Peer{msg.source()}, Peer{msg.destination()},
                                           write<message::Metadata>(marsKeys)},
                           std::move(buf)};
        }));
    }
}

void EncodeMtg2::print(std::ostream& os) const {
    os << "EncodeMtg2(";
    os << "options=";
    util::print(os, conf_);
    os << ")";
}

static ActionBuilder<EncodeMtg2> EncodeMtg2Builder("encode-mtg2");

}  // namespace multio::action
