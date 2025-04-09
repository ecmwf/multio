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


#include "multio/action/interpolate-fesom/InterpolateFesom.h"

#include <cmath>
#include <iomanip>
#include <set>
#include <string>
#include <vector>

#include "eckit/exception/Exceptions.h"


#include "InterpolateFesom_debug.h"
#include "eckit/filesystem/PathName.h"
#include "multio/LibMultio.h"
#include "multio/message/Message.h"
#include "multio/util/PrecisionTag.h"
#include "multio/util/Substitution.h"


namespace multio::action::interpolateFESOM {

namespace {

// TODO: Add the metadata we want to remove from the FESOM metadata
const std::set<std::string> metadata_black_list{
    "precision", "unstructuredGridType", "unstructuredGridSubtype", "gridType", "domain", "globalSize",
    "precision", "uuidOfHGrid"};


void fill_metadata(const message::Metadata& in_md, message::Metadata& out_md, size_t NSide,
                   orderingConvention_e orderingConvention, size_t globalSize, util::PrecisionTag outputPrecision,
                   double missingValue) {
    INTERPOLATE_FESOM_OUT_STREAM << " - enter fill_metadata" << std::endl;
    for (auto& keyPair : in_md) {
        if (metadata_black_list.find(keyPair.first) == metadata_black_list.cend()) {
            out_md.set(keyPair.first, keyPair.second);
        }
    }
    out_md.set("gridType", "healpix");
    out_md.set("orderingConvention", orderingConvention_enum2string(orderingConvention));
    out_md.set<std::int64_t>("Nside", NSide);
    out_md.set<std::int64_t>("globalSize", globalSize);
    out_md.set("precision", outputPrecision == util::PrecisionTag::Float ? "single" : "double");
    out_md.set<bool>("bitmapPresent", true);
    out_md.set("missingValue", missingValue);
    INTERPOLATE_FESOM_OUT_STREAM << " - exit fill_metadata" << std::endl;
};

// TODO: Consider removing this function!
// fname comes from an already expanded configuration.
std::string fullFileName(const std::string& fname) {
    if (fname != "none") {
        const std::string fullFname = util::replaceCurly(fname);
        eckit::PathName tmp{fullFname};
        if (!tmp.exists()) {
            std::ostringstream os;
            os << "File/path not found: " << fullFname << std::endl;
            throw eckit::SeriousBug(os.str(), Here());
        }
        return fullFname;
    }
    else {
        return "none";
    }
}

}  // namespace


template <typename T>
InterpolateFesom<T>::InterpolateFesom(const ComponentConfiguration& compConf) :
    ChainedAction{compConf},
    NSide_{static_cast<size_t>(compConf.parsedConfig().getLong("nside"))},
    orderingConvention_{
        orderingConvention_string2enum(compConf.parsedConfig().getString("ordering-convention", "ring"))},
    missingValue_{static_cast<T>(compConf.parsedConfig().getDouble("missing-value"))},
    outputPrecision_{compConf.parsedConfig().getString("output-precision", "from-message")},
    cachePath_{fullFileName(compConf.parsedConfig().getString("cache-path", "."))} {
    INTERPOLATE_FESOM_OUT_STREAM << " - InterpolateFesom :: enter constructor" << std::endl;
    if (outputPrecision_ != "single" && outputPrecision_ != "double" && outputPrecision_ != "from-message") {
        std::ostringstream os;
        os << " - Wrong value for output precision,"
           << "expected one of: [single|double|from-message]"
           << ", got: " << outputPrecision_ << std::endl;
        throw eckit::UserError(os.str(), Here());
    }
    INTERPOLATE_FESOM_OUT_STREAM << " - InterpolateFesom :: exit constructor" << std::endl;
};


template <typename T>
std::string InterpolateFesom<T>::generateKey(const message::Message& msg) const {
    INTERPOLATE_FESOM_OUT_STREAM << " - InterpolateFesom :: enter generateKey" << std::endl;
    // TODO: Probably missing the kind of fesom grid in the name
    //       neeed to see the metadata to understand how to extract it
    size_t level = static_cast<size_t>(                             //
        msg.metadata().getOpt<std::int64_t>("level").value_or(      //
            msg.metadata().getOpt<double>("levelist").value_or(0))  //
    );
    if ((msg.metadata().get<std::string>("category") == "ocean-3d")
        && (msg.metadata().get<std::string>("fesomLevelType") == "level")) {
        if (level == 0) {
            std::ostringstream os;
            os << " - Wrong level for the oceal level" << std::endl;
            throw eckit::SeriousBug(os.str(), Here());
        }
        level--;
    }
    auto searchUnstructuredGridType = msg.metadata().find("unstructuredGridType");
    if (searchUnstructuredGridType == msg.metadata().end()) {
        std::ostringstream os;
        os << " - \"unstructuredGridType\" not present in the metadata" << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    // if (!msg.metadata().has("unstructuredGridSubtype")) {
    //     std::ostringstream os;
    //     os << " - \"unstructuredGridSubtype\" not present in the metadata" << std::endl;
    //     throw eckit::SeriousBug(os.str(), Here());
    // }
    std::string fesomGridName = searchUnstructuredGridType->second.get<std::string>();
    std::string key = fesomCacheName(fesomGridName, msg.domain(), (sizeof(T) == 4 ? "single" : "double"), NSide_,
                                     orderingConvention_, level);

    INTERPOLATE_FESOM_OUT_STREAM << " - Generating key for the field :: " << key << std::endl;
    INTERPOLATE_FESOM_OUT_STREAM << " - InterpolateFesom :: exit generateKey" << std::endl;
    return key;
}


template <typename T>
void InterpolateFesom<T>::executeImpl(message::Message msg) {
    INTERPOLATE_FESOM_OUT_STREAM
        << " ======================================================================================================= "
        << std::endl;
    INTERPOLATE_FESOM_OUT_STREAM << " - InterpolateFesom :: enter executeImpl" << std::endl;

    if (msg.tag() != message::Message::Tag::Field) {
        INTERPOLATE_FESOM_OUT_STREAM << " - exit executeImpl (on flush)" << std::endl;
        INTERPOLATE_FESOM_OUT_STREAM << " ============================================================================="
                                        "========================== "
                                     << std::endl;
        executeNext(msg);
        return;
    }

    std::string key = generateKey(msg);
    if (Interpolators_.find(key) == Interpolators_.end()) {
        // no need to check for grid type since it is already checked in the generateKey function
        Interpolators_[key] = std::make_unique<Fesom2HEALPix<T>>(
            msg, cachePath_, msg.metadata().get<std::string>("unstructuredGridType"), NSide_, orderingConvention_);
    }

    executeNext(util::dispatchPrecisionTag(msg.precision(), [&](auto in_pt) -> message::Message {
        util::PrecisionTag opt
            = (outputPrecision_ == "from-message" ? msg.precision() : util::decodePrecisionTag(outputPrecision_));
        return util::dispatchPrecisionTag(opt, [&](auto out_pt) -> message::Message {
            using InputPrecision = typename decltype(in_pt)::type;
            using OutputPrecision = typename decltype(out_pt)::type;
            std::vector<OutputPrecision> outData;
            message::Metadata md;
            size_t inputSize = msg.payload().size() / sizeof(InputPrecision);
            size_t outputSize = 12 * NSide_ * NSide_;
            outData.resize(outputSize);
            const InputPrecision* val = static_cast<const InputPrecision*>(msg.payload().data());
            Interpolators_.at(key)->interpolate(val, outData.data(), inputSize, outputSize,
                                                static_cast<OutputPrecision>(missingValue_));
            eckit::Buffer buffer(reinterpret_cast<const char*>(outData.data()),
                                 outData.size() * sizeof(OutputPrecision));
            fill_metadata(msg.metadata(), md, NSide_, orderingConvention_, outData.size(), opt, missingValue_);
            INTERPOLATE_FESOM_OUT_STREAM << " - InterpolateFesom :: Interpolation results:" << std::endl;
            INTERPOLATE_FESOM_OUT_STREAM << "       * FROM: " << msg.metadata() << " " << std::endl;
            INTERPOLATE_FESOM_OUT_STREAM << "       * TO  :" << md << std::endl;
            INTERPOLATE_FESOM_OUT_STREAM << " - exit executeImpl (on field) " << std::endl;
            INTERPOLATE_FESOM_OUT_STREAM << " ========================================================================="
                                            "============================== "
                                         << std::endl;
            INTERPOLATE_FESOM_OUT_STREAM << std::endl << std::endl;
            return {
                message::Message::Header{message::Message::Tag::Field, msg.source(), msg.destination(), std::move(md)},
                std::move(buffer)};
        });
    }));
}


template <typename T>
void InterpolateFesom<T>::print(std::ostream& os) const {
    os << "interpolate-fesom-" << (sizeof(T) == 4 ? "single" : "double");
}


static ActionBuilder<InterpolateFesom<float>> InterpolateFesomBuilderSP("interpolate-fesom-single");
static ActionBuilder<InterpolateFesom<double>> InterpolateFesomBuilderDP("interpolate-fesom-double");


}  // namespace multio::action::interpolateFESOM
