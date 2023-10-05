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


#include "multio/action/interpolate-fesom/InterpolateFESOM.h"

#include <string>
#include <vector>
#include <cmath>
#include <iomanip>

#include "eckit/exception/Exceptions.h"


#include "InterpolateFESOM_debug.h"
#include "multio/LibMultio.h"
#include "multio/message/Message.h"
#include "multio/util/PrecisionTag.h"
#include "multio/util/Substitution.h"
#include "eckit/filesystem/PathName.h"


namespace multio::action::interpolateFESOM {

namespace {

// TODO: Add the metadata we want to remove from the FESOM metadata
const std::vector<std::string> metadata_black_list{"precision", "grid-type", "gridSubtype", "globalSize", "precision"};


template <typename DestType>
void forwardMetadata(const eckit::LocalConfiguration& cfg, DestType& destination, const std::string& key,
                     const eckit::Value& value) {
    INTERPOLATE_FESOM_OUT_STREAM << " - enter forward_metadata" << std::endl;
    if (value.isList()) {
        if (value.head().isDouble()) {
            destination.set(key, cfg.getDoubleVector(key));
        }
        else if (value.head().isNumber()) {
            destination.set(key, cfg.getLongVector(key));
        }
        else if (value.isString()) {
            destination.set(key, cfg.getStringVector(key));
        }
        else {
            NOTIMP;
        }
        return;
    }
    if (value.isBool()) {
        destination.set(key, cfg.getBool(key));
    }
    else if (value.isDouble()) {
        destination.set(key, cfg.getDouble(key));
    }
    else if (value.isNumber()) {
        destination.set(key, cfg.getInt(key));
    }
    else if (value.isString()) {
        destination.set(key, cfg.getString(key).c_str());
    }
    else {
        NOTIMP;
    }
    INTERPOLATE_FESOM_OUT_STREAM << " - exit forward_metadata" << std::endl;
};


void fill_metadata(const message::Metadata& in_md, message::Metadata& out_md, size_t NSide,
                   orderingConvention_e orderingConvention, size_t globalSize, util::PrecisionTag outputPrecision, double missingValue) {
    INTERPOLATE_FESOM_OUT_STREAM << " - enter fill_metadata" << std::endl;
    for (auto& key : in_md.keys()) {
        if (std::find(metadata_black_list.cbegin(), metadata_black_list.cend(), key) == metadata_black_list.cend()) {
            forwardMetadata<message::Metadata>(in_md, out_md, key, in_md.getSubConfiguration(key).get());
        }
    }
    out_md.set("gridType", "HEALPix");
    out_md.set("orderingConvention", orderingConvention_enum2string(orderingConvention));
    out_md.set("Nside", NSide);
    out_md.set("globalSize", globalSize);
    out_md.set("precision", outputPrecision == util::PrecisionTag::Float ? "single" : "double");
    out_md.set("bitmapPresent",1);
    out_md.set("missingValue",missingValue);
    INTERPOLATE_FESOM_OUT_STREAM << " - exit fill_metadata" << std::endl;
    return;
};

std::string fullFileName( const std::string& fname ){
    if ( fname != "none" ){
        const std::string fullFname = util::replaceCurly(fname, [](std::string_view replace) {
             std::string lookUpKey{replace};
             char* env = ::getenv(lookUpKey.c_str());
             return env ? std::optional<std::string>{env} : std::optional<std::string>{};    
        });
        eckit::PathName tmp{fullFname};
        if ( !tmp.exists() ){
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
InterpolateFESOM<T>::InterpolateFESOM(const ComponentConfiguration& compConf) :
    ChainedAction{compConf},
    NSide_{static_cast<size_t>(compConf.parsedConfig().getLong("nside"))},
    orderingConvention_{
        orderingConvention_string2enum(compConf.parsedConfig().getString("ordering-convention", "ring"))},
    missingValue_{static_cast<T>(compConf.parsedConfig().getDouble("missing-value"))},
    outputPrecision_{compConf.parsedConfig().getString("output-precision", "from-message")},
    cachePath_{fullFileName(compConf.parsedConfig().getString("cache-path", "."))} {
    INTERPOLATE_FESOM_OUT_STREAM << " - InterpolateFESOM :: enter constructor" << std::endl;
    if ( outputPrecision_ != "single" && outputPrecision_ != "double" && outputPrecision_ != "from-message") {
        std::ostringstream os;
        os << " - Wrong value for output precision,"
           << "expected one of: [single|double|from-message]"
           << ", got: " << outputPrecision_
           << std::endl;
        throw eckit::UserError(os.str(), Here());
    } 
    INTERPOLATE_FESOM_OUT_STREAM << " - InterpolateFESOM :: exit constructor" << std::endl;
};


template <typename T>
std::string InterpolateFESOM<T>::generateKey(const message::Message& msg) const {
    INTERPOLATE_FESOM_OUT_STREAM << " - InterpolateFESOM :: enter generateKey" << std::endl;
    // TODO: Probably missing the kind of fesom grid in the name
    //       neeed to see the metadata to understand how to extract it
    size_t level = static_cast<size_t>(msg.metadata().getLong("level", msg.metadata().getDouble("levelist", 0)));
    if ( (msg.metadata().getString("category" )       == "ocean-3d") && 
         (msg.metadata().getString("fesomLevelType" ) == "level") ) {
        if (level == 0) {
            std::ostringstream os;
            os << " - Wrong level for the oceal level" << std::endl;
            throw eckit::SeriousBug(os.str(), Here());
        }        
        level--;    
    }
    if ( !msg.metadata().has("gridType")){
        std::ostringstream os;
        os << " - \"gridType\" not present in the metadata" << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    if ( !msg.metadata().has("gridSubType")){
        std::ostringstream os;
        os << " - \"gridSubType\" not present in the metadata" << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    std::string fesomGridName = msg.metadata().getString("gridType");
    const std::string domain = msg.metadata().getString("gridSubType");
    std::string key = fesomCacheName( fesomGridName, domain, (sizeof(T) == 4 ? "single" : "double"), NSide_, orderingConvention_, level );

    INTERPOLATE_FESOM_OUT_STREAM << " - Generating key for the field :: " << key << std::endl;
    INTERPOLATE_FESOM_OUT_STREAM << " - InterpolateFESOM :: exit generateKey" << std::endl;
    return key;
}


template <typename T>
void InterpolateFESOM<T>::executeImpl(message::Message msg) {
    INTERPOLATE_FESOM_OUT_STREAM
        << " ======================================================================================================= "
        << std::endl;
    INTERPOLATE_FESOM_OUT_STREAM << " - InterpolateFESOM :: enter executeImpl" << std::endl;

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
        Interpolators_[key]
            = std::make_unique<Fesom2HEALPix<T>>(msg, cachePath_, msg.metadata().getString("gridType"), NSide_, orderingConvention_);
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
            INTERPOLATE_FESOM_OUT_STREAM << " - InterpolateFESOM :: Interpolation results:" << std::endl;
            INTERPOLATE_FESOM_OUT_STREAM << "       * FROM: " << msg.metadata() << " " << std::endl;
            INTERPOLATE_FESOM_OUT_STREAM << "       * TO  :" << md << std::endl;
            INTERPOLATE_FESOM_OUT_STREAM << " - exit executeImpl (on field) " << std::endl;
            INTERPOLATE_FESOM_OUT_STREAM << " ========================================================================="
                                            "============================== "
                                         << std::endl;
            INTERPOLATE_FESOM_OUT_STREAM << std::endl
                                         << std::endl;
            return {
                message::Message::Header{message::Message::Tag::Field, msg.source(), msg.destination(), std::move(md)},
                std::move(buffer)};
        });
    }));
}


template <typename T>
void InterpolateFESOM<T>::print(std::ostream& os) const {
    os << "interpolate-fesom-" << (sizeof(T) == 4 ? "single" : "double");
}


static ActionBuilder<InterpolateFESOM<float>> InterpolateFesomBuilderSP("interpolate-fesom-single");
static ActionBuilder<InterpolateFESOM<double>> InterpolateFesomBuilderDP("interpolate-fesom-double");


}  // namespace multio::action::interpolateFESOM
