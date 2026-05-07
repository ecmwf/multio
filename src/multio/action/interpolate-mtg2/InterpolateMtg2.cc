/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 *
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


#include "multio/action/interpolate-mtg2/InterpolateMtg2.h"

#include <algorithm>
#include <array>
#include <iomanip>
#include <regex>
#include <sstream>
#include <string>
#include <vector>

#include "eckit/exception/Exceptions.h"
#include "eckit/mpi/Comm.h"
#include "eckit/parser/YAMLParser.h"
#include "eckit/types/Fraction.h"

#include "metkit/mars/MarsLanguage.h"

#include "mir/api/MIRJob.h"
#include "mir/input/RawInput.h"
#include "mir/output/ResizableOutput.h"
#include "mir/param/GridSpecParametrisation.h"
#include "mir/param/RuntimeParametrisation.h"
#include "mir/param/SimpleParametrisation.h"
#include "mir/repres/gauss/reduced/Reduced.h"

#include "multio/LibMultio.h"
#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/Glossary.h"
#include "multio/datamod/GribKeys.h"
#include "multio/datamod/MarsKeys.h"
#include "multio/datamod/core/EntryParser.h"
#include "multio/message/Message.h"
#include "multio/util/PrecisionTag.h"
#include "multio/util/Substitution.h"
#include "multio/util/config/Parser.h"


namespace multio::action::interpolate_mtg2 {

namespace dm = multio::datamod;

using message::MetadataTypes;

namespace {

// Builds a path to a matrix file with the pattern {cache_path}/{fromGrid}_to_{toGrid}_double_{levelist || 0}.mat
std::string generateKey(const message::Message& msg, const std::string& cache_path, const std::string& fromGrid,
                        const std::string& toGrid) {
    auto levelist = dm::parseEntry(dm::LEVELIST, msg.metadata());

    if (!levelist.isSet()) {
        levelist.set(0l);
    }

    std::ostringstream key;
    key << cache_path << "/";
    key << fromGrid << "_to_" << toGrid;
    key << "_double";
    key << "_" << levelist.get();
    key << ".mat";

    return key.str();
}

}  // namespace


InterpolateMtg2::InterpolateMtg2(const ComponentConfiguration& compConf) :
    ChainedAction(compConf), opts_{cf::parseActionConfig<Options>(compConf)} {}


void prepareOutputMetadata(const OutputConfig& outCfg, const message::Metadata& mdIn, message::Metadata& md) {
    for (const auto& kv : mdIn) {
        md.set(kv.first, kv.second);
    }

    md.set(dm::legacy::Precision, "double");

    if (outCfg.additionalMetadata.resolution) {
        for (const auto& pair : cf::detail::EnumTrait<ResolutionKeyword>::values) {
            if (pair.first == *outCfg.additionalMetadata.resolution) {
                md.set(dm::RESOLUTION.key(), pair.second);
                break;
            }
        }
    }

    md.set("grid", outCfg.grid);
};

template <>
void InterpolateMtg2::interpolateMessage<double>(message::Message&& msg) const {
    LOG_DEBUG_LIB(LibMultio) << "InterpolateMtg2 :: Metadata of the input message :: " << std::endl
                             << msg.metadata() << std::endl
                             << std::endl;


    const double* data = reinterpret_cast<const double*>(msg.payload().data());
    const size_t size = msg.payload().size() / sizeof(double);

    // Prepare input once
    const auto& grid = dm::parseEntry(dm::GRID, msg.metadata());
    const auto& bitmapPresent = dm::parseEntry(dm::BitmapPresent, msg.metadata());
    const auto& missingValue = dm::parseEntry(dm::MissingValue, msg.metadata());

    if (!grid.isSet()) {
        std::ostringstream oss;
        oss << "Input metadata does not have a key \"grid\": " << msg.metadata();
        throw eckit::UserError(oss.str(), Here());
    }

    // mir::param::GridSpecParametrisation inputPar(std::string("{\"grid\":\"") + grid.get() + std::string("\"}"));
    mir::param::GridSpecParametrisation gridSpecPar(grid.get());
    mir::param::RuntimeParametrisation inputPar(gridSpecPar);

    /// Not possible with GridSpecParametrisation
    if (missingValue.isSet() && bitmapPresent.isSet() && bitmapPresent.get()) {
        inputPar.set("missing_value", missingValue.get());
    }

    mir::input::RawInput input(data, size, inputPar);
    LOG_DEBUG_LIB(LibMultio) << "InterpolateMtg2 :: input :: " << std::endl << inputPar << std::endl << std::endl;


    for (const OutputConfig& outCfg : opts_.outputs) {
        if (!outCfg.enable)
            continue;

        std::vector<double> outData;
        mir::param::SimpleParametrisation outMetadata;
        mir::output::ResizableOutput output(outData, outMetadata);

        message::Metadata md;
        prepareOutputMetadata(outCfg, msg.metadata(), md);

        mir::api::MIRJob job;
        job.set("grid", outCfg.grid);

        /// Forward MIROptions
        util::forEach(
            [&](const auto& entry) {
                const auto& opt = entry.get(outCfg.options);
                if (opt) {
                    job.set(std::string(entry.key), *opt);
                }
            },
            MIROptions::fields_);

        // Special case that initially used for fesom. Load level dependent weight matrices by using a generic key
        // as matrix file with the pattern {cache_path}/{fromGrid}_to_{toGrid}_double_{levelist || 0}.mat
        if (outCfg.options.interpolation && *outCfg.options.interpolation == "matrix"
            && !outCfg.options.interpolationMatrix) {
            /// TODO(pgeier)  Check if this section is still required with new MIR for ClimateDT
            ///               The key is more generic now instead of fesom -> HEALPix specific.
            ///               Pedro suggestion is to use intermediate-interpolation=nearest-neighbour and intgrid=OXXX
            job.set("interpolation-matrix", generateKey(msg, outCfg.cachePath, grid.get(), outCfg.grid));
        }

        LOG_DEBUG_LIB(LibMultio) << "InterpolateMtg2 :: job " << std::endl << job << std::endl << std::endl;

        // TODO: Probably this operation needs to be done when plans are called, in this way
        //       it is valid for all the IO actions as it should be
        auto& originalComm = eckit::mpi::comm();
        eckit::mpi::setCommDefault("self");
        job.execute(input, output);
        eckit::mpi::setCommDefault(originalComm.name().c_str());

        md.set<std::int64_t>(dm::legacy::GlobalSize, outData.size());
        if (outMetadata.has("missing_value")) {
            double v;
            outMetadata.get("missing_value", v);
            md.set(dm::MissingValue.key(), v);
            md.set(dm::BitmapPresent.key(), true);
        }

        LOG_DEBUG_LIB(LibMultio) << "InterpolateMtg2 :: Metadata of the output message :: " << std::endl
                                 << md << std::endl
                                 << std::endl;

        executeNext(message::Message{
            message::Message::Header{message::Message::Tag::Field, msg.source(), msg.destination(), std::move(md)},
            eckit::Buffer{reinterpret_cast<const char*>(outData.data()), outData.size() * sizeof(double)}});
    }
}

template <>
void InterpolateMtg2::interpolateMessage<float>(message::Message&& msg) const {
    // convert single/double precision, interpolate, convert double/single
    interpolateMessage<double>(message::convert_precision<float, double>(std::move(msg)));
}

void InterpolateMtg2::executeImpl(message::Message msg) {
    if (msg.tag() != message::Message::Tag::Field) {
        executeNext(msg);
        return;
    }

    util::dispatchPrecisionTag(msg.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        interpolateMessage<Precision>(std::move(msg));
    });
}


void InterpolateMtg2::print(std::ostream& os) const {
    os << "InterpolateMtg2";
}


static ActionBuilder<InterpolateMtg2> InterpolateMtg2Builder("interpolate-mtg2");


}  // namespace multio::action::interpolate_mtg2
