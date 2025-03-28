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
#include "multio/config/PathConfiguration.h"
#include "multio/util/MioGribHandle.h"
#include "multio/util/PrecisionTag.h"
#include "multio/message/Glossary.h"

namespace multio::action {

using config::configuration_path_name;

using message::glossary;
using message::Message;
using message::MetadataTypes;
using message::Peer;


namespace {

EncodeMultiOMOptions parseOptions(const ComponentConfiguration& compConf) {
    EncodeMultiOMOptions ret;
    const auto& conf = compConf.parsedConfig();

    if (conf.has("knowledge-root")) {
        ret.knowledgeRoot = conf.getString("knowledge-root");
    }
    else {
        ret.knowledgeRoot = multio::LibMultio::instance().libraryHome();
    }

    if (conf.has("samples-path")) {
        ret.samplesPath = conf.getString("samples_path");
    }
    else {
        ret.samplesPath = ret.knowledgeRoot.value() + "/share/multiom/samples";
    }

    if (conf.has("mapping-rules")) {
        ret.mappingFile = conf.getString("mapping-rules");
    }
    else {
        ret.mappingFile = ret.knowledgeRoot.value() + "/share/multiom/encodings/encoding-rules.yaml";
    }

    if (conf.has("encoding-rules")) {
        ret.encodingFile = conf.getString("encoding-rules");
    }
    else {
        ret.encodingFile = ret.knowledgeRoot.value() + "/share/multiom/mappings/mapping-rules.yaml";
    }

    return ret;
}

MultiOMEncoder makeEncoder(const EncodeMultiOMOptions& opts, const ComponentConfiguration& conf) {
    MultiOMDict optDict(MultiOMDictKind::Options);
    if (opts.samplesPath) {
        optDict.set("samples-path", opts.samplesPath->c_str());
    }
    if (opts.encodingFile) {
        optDict.set("encoding-rules", opts.encodingFile->c_str());
    }
    if (opts.mappingFile) {
        optDict.set("mapping-rules", opts.mappingFile->c_str());
    }

    // TODO -- in the tool we used to set IFS_INSTALL_DIR ... now we expect the user to set it
    // setenv("IFS_INSTALL_DIR", knowledgeRoot_.c_str(), 0);

    return MultiOMEncoder(optDict);
}

std::string encodingExceptionReason(const std::string& r) {
    std::string s("Mtg2 Enocding exception: ");
    s.append(r);
    return s;
}
}  // namespace


std::string multiOMDictKindString(MultiOMDictKind kind) {
    switch (kind) {
        case MultiOMDictKind::Options:
            return "options";
        case MultiOMDictKind::MARS:
            return "mars";
        case MultiOMDictKind::Parametrization:
            return "parametrization";
        case MultiOMDictKind::ReducedGG:
            return "reduced-gg";
        case MultiOMDictKind::RegularLL:
            return "regular-ll";
        case MultiOMDictKind::SH:
            return "sh";
        default:
            NOTIMP;
    }
}

MultiOMDict::MultiOMDict(MultiOMDictKind kind): kind_{kind} {
    std::string kindStr = multiOMDictKindString(kind);
    ASSERT(multio_grib2_dict_create(&dict_, kindStr.data()) == 0);

    if (kind == MultiOMDictKind::Options) {
        ASSERT(multio_grib2_init_options(&dict_) == 0);
    }
}

void MultiOMDict::toYAML(const std::string& file) {
    multio_grib2_dict_to_yaml(&dict_, "stdout");
}

void MultiOMDict::set(const char* key, const char* val) {
    ASSERT(multio_grib2_dict_set(dict_, key, val) == 0);
}
void MultiOMDict::set(const std::string& key, const std::string& val) {
    set(key.c_str(), val.c_str());
}
void MultiOMDict::set_geometry(MultiOMDict& geom) {
    ASSERT(kind_ == MultiOMDictKind::Parametrization);
    switch (geom.kind_) {
        case MultiOMDictKind::ReducedGG:
        case MultiOMDictKind::RegularLL:
        case MultiOMDictKind::SH:
            ASSERT(multio_grib2_dict_set_geometry(dict_, geom.dict_) == 0);
        default:
            throw EncodeMtg2Exception("Passed dict is not a geometry dictt", Here());
    }
}


void MultiOMDict::set(const std::string& key, std::int64_t val) {
    ASSERT(multio_grib2_dict_set_int64(dict_, key.c_str(), val) == 0);
}
void MultiOMDict::set(const std::string& key, double val) {
    ASSERT(multio_grib2_dict_set_double(dict_, key.c_str(), val) == 0);
}
void MultiOMDict::set(const std::string& key, const std::int64_t* val, std::size_t len) {
    ASSERT(multio_grib2_dict_set_int64_array(dict_, key.c_str(), val, len) == 0);
}
void MultiOMDict::set(const std::string& key, const double* val, std::size_t len) {
    ASSERT(multio_grib2_dict_set_double_array(dict_, key.c_str(), val, len) == 0);
}
void MultiOMDict::set(const std::string& key, const std::vector<std::int64_t>& val) {
    set(key, val.data(), val.size());
}
void MultiOMDict::set(const std::string& key, const std::vector<double>& val) {
    set(key, val.data(), val.size());
}


MultiOMDict::~MultiOMDict() {
    ASSERT(multio_grib2_dict_destroy(&dict_) == 0);
}

void* MultiOMDict::get() {
    return dict_;
}


MultiOMEncoder::MultiOMEncoder(MultiOMDict& options) {
    ASSERT(multio_grib2_encoder_open(options.get(), &encoder_) == 0);
}

std::unique_ptr<codes_handle> MultiOMEncoder::encode(MultiOMDict& mars, MultiOMDict& par, const double* data,
                                                     std::size_t len) {
    codes_handle* rawOutputCodesHandle = nullptr;
    ASSERT(multio_grib2_encoder_encode64(encoder_, mars.get(), par.get(), data, len, (void**)&rawOutputCodesHandle));
    return std::unique_ptr<codes_handle>{rawOutputCodesHandle};
}

std::unique_ptr<codes_handle> MultiOMEncoder::encode(MultiOMDict& mars, MultiOMDict& par, const float* data,
                                                     std::size_t len) {
    codes_handle* rawOutputCodesHandle = nullptr;
    ASSERT(multio_grib2_encoder_encode32(encoder_, mars.get(), par.get(), data, len, (void**)&rawOutputCodesHandle));
    return std::unique_ptr<codes_handle>{rawOutputCodesHandle};
}


MultiOMEncoder::~MultiOMEncoder() {
    ASSERT(multio_grib2_encoder_close(&encoder_) == 0);
}


EncodeMtg2Exception::EncodeMtg2Exception(const std::string& r, const eckit::CodeLocation& l) :
    eckit::Exception(encodingExceptionReason(r), l) {}


EncodeMtg2::EncodeMtg2(const ComponentConfiguration& compConf) :
    ChainedAction{compConf}, options_{parseOptions(compConf)}, encoder_{makeEncoder(options_, compConf)} {}
    

namespace {


void handleGeometryGG(MultiOMDict& mars, const message::Metadata& md, const std::string& keyPrefix) {
}
void handleGeometrySH(MultiOMDict& mars, const message::Metadata& md, const std::string& keyPrefix) {
}
void handleGeometryLL(MultiOMDict& mars, const message::Metadata& md, const std::string& keyPrefix) {
}

void handleGeometry(MultiOMDict& mars, const message::Metadata& md) {
    using namespace message::Mtg2;
}

}

void EncodeMtg2::executeImpl(Message msg) {
    if (msg.tag() != Message::Tag::Field) {
        executeNext(std::move(msg));
        return;
    }
    
    auto& md = msg.metadata();

    // TO encoding
    MultiOMDict mars{MultiOMDictKind::MARS};
    MultiOMDict par{MultiOMDictKind::Parametrization};
    
    {
        using namespace message::Mtg2;
        withMarsKeys([&](const auto& kvDescr){
            if (auto search = md.find(kvDescr); search != md.end()) {
                mars.set(kvDescr, kvDescr.get(search->second));
            }
        });
        
        withParametrizationKeys([&](const auto& kvDescrPrefixed, const auto& kvDescr){
            if (auto search = md.find(kvDescrPrefixed); search != md.end()) {
                par.set(kvDescr, kvDescrPrefixed.get(search->second));
            }
        });
        
        auto grid = mars::grid.get(md);
        Repres repres;
        std::string prefix;
        std::tie(repres, prefix) = represAndPrefixFromGridName(grid);
        
        MultiOMDict geom{
            ([&](){
                switch (repres) {
                    case Repres::GG:
                        return MultiOMDictKind::ReducedGG;
                    case Repres::LL:
                        return MultiOMDictKind::RegularLL;
                    case Repres::SH:
                        return MultiOMDictKind::SH;
                }
                throw EncodeMtg2Exception("unkown repres", Here());
            })()
        };
        
        withGeometryKeys(repres, [&](const auto& kvDescr) {
            if (auto search = md.find(prefix + std::string(kvDescr)); search != md.end()) {
                geom.set(kvDescr, kvDescr.get(search->second));
            }
        });
        par.set_geometry(geom);
    }
    
    

    auto& payload = msg.payload();

    // auto beg = reinterpret_cast<const T*>(msg.payload().data());
    // this->setDataValues(beg, msg.globalSize());

    // msg.header().acquireMetadata();
    // const auto& metadata = msg.metadata();

    executeNext(dispatchPrecisionTag(msg.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        auto rawGrib2Handle = encoder_.encode(mars, par, static_cast<const Precision*>(payload.data()),
                                              payload.size() / sizeof(Precision));

        // Create non-owning grib handle by passing by reference
        util::MioGribHandle gribHandle{*rawGrib2Handle.get()};

        // Initialize buffer with length
        eckit::Buffer buf{gribHandle.length()};
        gribHandle.write(buf);

        return Message{Message::Header{Message::Tag::Field, Peer{msg.source().group()}, Peer{msg.destination()}},
                       std::move(buf)};
    }));
}

void EncodeMtg2::print(std::ostream& os) const {
    os << "EncodeMtg2(knowledege-root=" << options_.knowledgeRoot.value_or("") << ", ";
    os << "samples-path=" << options_.samplesPath.value_or("") << ", ";
    os << "mapping-rules=" << options_.mappingFile.value_or("") << ", ";
    os << "encoding-rules=" << options_.encodingFile.value_or("");
    os << ")";
}


static ActionBuilder<EncodeMtg2> EncodeMtg2Builder("encode-mtg2");

}  // namespace multio::action
