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
#include "multio/action/encode-mtg2/EncoderCache.h"
#include "multio/action/encode-mtg2/Options.h"
#include "multio/config/PathConfiguration.h"
#include "multio/datamod/Glossary.h"
#include "multio/datamod/MarsMiscGeo.h"
#include "multio/message/Parametrization.h"
#include "multio/util/MioGribHandle.h"
#include "multio/util/PrecisionTag.h"

namespace multio::action {

using message::Message;
using message::Peer;


EncodeMtg2::EncodeMtg2(const ComponentConfiguration& compConf) :
    ChainedAction{compConf}, conf_{datamod::readByValue(EncodeMtg2KeySet{}, compConf.parsedConfig())}, cache_{conf_} {}


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

        // @Mirco here we get the cached raw encoder
        std::unique_ptr<util::MioGribHandle> sample = cache_.getSample(marsKeys, miscKeys, geoKeys);

        executeNext(dispatchPrecisionTag(msg.precision(), [&](auto pt) {
            using Precision = typename decltype(pt)::type;

            auto beg = reinterpret_cast<const Precision*>(msg.payload().data());
            sample->setDataValues(beg, msg.payload().size()/sizeof(Precision));

            // msg.header().acquireMetadata();
            // const auto& metadata = msg.metadata();
            // auto offsetByValue = metadata.getOpt<double>("offsetValuesBy");
            // if (offsetByValue) {
            //     setValue("offsetValuesBy", *offsetByValue);
            // }

            eckit::Buffer buf{sample->length()};
            sample->write(buf);

            return Message{Message::Header{Message::Tag::Field, Peer{msg.source().group()}, Peer{msg.destination()}},
                           std::move(buf)};
        }));
    }
}

void EncodeMtg2::print(std::ostream& os) const {
    os << "EncodeMtg2(";
    os << "options=" << conf_;
    os << ")";
}

static ActionBuilder<EncodeMtg2> EncodeMtg2Builder("encode-mtg2");

}  // namespace multio::action
