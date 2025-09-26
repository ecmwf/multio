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
#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/MarsMiscGeo.h"
#include "multio/datamod/core/EntryParser.h"
#include "multio/datamod/core/Record.h"
#include "multio/mars2grib/api/RawAPI.h"
#include "multio/mars2mars/Rules.h"
#include "multio/message/Parametrization.h"
#include "multio/util/MioGribHandle.h"
#include "multio/util/PrecisionTag.h"
#include "multio/util/Print.h"

namespace multio::action {

namespace dm = multio::datamod;

using message::Message;
using message::Peer;

mars2grib::RawOptions mapOpts(EncodeMtg2Options opts) {
    mars2grib::RawOptions ret;
    ret.cached = opts.cached.get();
    return ret;
};

EncodeMtg2Options parseOpts(const ComponentConfiguration& compConf) {
    /// TODO(pgeier) With C++20 designators are more useful for inline creation of structs:
    /// ParsedOptions{.allowAdditionalKeys=false}
    dm::ParseOptions opts;
    opts.allowAdditionalKeys = false;
    
    // TODO(pgeier) Fix after refactoring action - need to remove keys "type" and "next"
    auto conf = compConf.parsedConfig();
    conf.remove("type");
    conf.remove("next");
    
    return dm::readRecordByValue<EncodeMtg2Options>(conf, opts);
}


EncodeMtg2::EncodeMtg2(const ComponentConfiguration& compConf) :
    ChainedAction{compConf}, opts_{parseOpts(compConf)}, mars2grib_{mapOpts(opts_)} {}


void EncodeMtg2::executeImpl(Message msg) {
    if (msg.tag() != Message::Tag::Field) {
        executeNext(std::move(msg));
        return;
    }

    auto& md = msg.metadata();

    {
        // Read and set unscoped mars keys
        auto marsRec = dm::readRecord<dm::FullMarsRecord>(md);

        // Read scoped misc keys
        auto scopedMiscRec = dm::scopeRecord(dm::MiscRecord{});
        dm::readRecord(scopedMiscRec, md);
        // Write unscoped misc keys
        auto miscRec = dm::unscopeRecord(std::move(scopedMiscRec));

        auto scopedGeo = dm::getGeometryRecord(marsRec);

        // If grid.. check if atlas is given.
        if (marsRec.grid.isSet()) {
            std::string scope{std::visit([](const auto& k) { return dm::getRecordScope(k); }, scopedGeo)};
            const auto& global = message::Parametrization::instance().get();
            // Fetch atlas and store in global parametrization (by scoping keys...)
            // Scoping here may be refactored
            if (opts_.geoFromAtlas.get() && (global.find(scope) == global.end())) {
                extract::AtlasGeoSetter::handleGrid(scope, marsRec.grid.get());
            }
        }

        // Read & unscope geo keys from metadata
        auto geo = std::visit(
            [&](auto&& geoRec) -> dm::Geometry {
                dm::readRecord(geoRec, md);
                return unscopeRecord(std::move(geoRec));
            },
            std::move(scopedGeo));


        // Apply mappings
        auto mappingResult = mars2mars::applyMappings(mars2mars::allRules(), marsRec, miscRec);


        std::unique_ptr<util::MioGribHandle> sample = mars2grib_.getHandle(marsRec, miscRec, geo);

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

            // The +32 is related to bug
            // TODO(pgeier) Track bug ECC-2130: https://jira.ecmwf.int/browse/ECC-2130
            eckit::Buffer buf{sample->length() + 32};
            sample->write(buf);

            // TODO(pgeier) write mapped metadata
            return Message{Message::Header{Message::Tag::Field, Peer{msg.source()}, Peer{msg.destination()},
                                           dm::dumpRecord<message::Metadata>(marsRec)},
                           std::move(buf)};
        }));
    }
}

void EncodeMtg2::print(std::ostream& os) const {
    util::PrintStream ps(os);
    ps << dm::RecordName_v<EncodeMtg2Options> << std::endl;
    {
        util::IndentGuard g{ps};
        ps << opts_;
    }
}

static ActionBuilder<EncodeMtg2> EncodeMtg2Builder(std::string(dm::RecordName_v<EncodeMtg2Options>));

}  // namespace multio::action
