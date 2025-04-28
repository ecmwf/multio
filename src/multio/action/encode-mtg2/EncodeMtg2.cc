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
#include "multio/message/Glossary.h"
#include "multio/util/MioGribHandle.h"
#include "multio/util/PrecisionTag.h"

namespace multio::action::encode_mtg2 {

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
        ret.mappingFile = ret.knowledgeRoot.value() + "/share/multiom/mappings/mapping-rules.yaml";
    }

    if (conf.has("encoding-rules")) {
        ret.encodingFile = conf.getString("encoding-rules");
    }
    else {
        ret.encodingFile = ret.knowledgeRoot.value() + "/share/multiom/encodings/encoding-rules.yaml";
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

MultiOMDict::MultiOMDict(MultiOMDictKind kind) : kind_{kind} {
    std::string kindStr = multiOMDictKindString(kind);
    void* dict = NULL;
    if (multio_grib2_dict_create(&dict, kindStr.data()) != 0) {
        throw EncodeMtg2Exception(std::string("Can not create dict kind ") + kindStr, Here());
    }

    if (kind == MultiOMDictKind::Options) {
        ASSERT(multio_grib2_init_options(&dict) == 0);
    }
    dict_.reset(static_cast<ForeignDictType*>(dict));
}

void MultiOMDict::toYAML(const std::string& file) {
    multio_grib2_dict_to_yaml(get(), "stdout");
}

void MultiOMDict::set(const char* key, const char* val) {
    if (multio_grib2_dict_set(get(), key, val) != 0) {
        throw EncodeMtg2Exception(
            std::string("Can not set key ") + std::string(key) + std::string(" with value ") + std::string(val),
            Here());
    }
}

void MultiOMDict::set(const std::string& key, const std::string& val) {
    set(key.c_str(), val.c_str());
}

void MultiOMDict::set_geometry(MultiOMDict&& geom) {
    ASSERT(kind_ == MultiOMDictKind::Parametrization);
    switch (geom.kind_) {
        case MultiOMDictKind::ReducedGG:
        case MultiOMDictKind::RegularLL:
        case MultiOMDictKind::SH:
            geom_ = std::make_unique<MultiOMDict>(std::move(geom));
            ASSERT(multio_grib2_dict_set_geometry(get(), geom_->get()) == 0);
            break;
        default:
            throw EncodeMtg2Exception("Passed dict is not a geometry dict", Here());
    }
}


void MultiOMDict::set(const std::string& key, std::int64_t val) {
    if (multio_grib2_dict_set_int64(get(), key.c_str(), val) != 0) {
        throw EncodeMtg2Exception(std::string("Can not set key ") + std::string(key) + std::string(" with int64 value ")
                                      + std::to_string(val),
                                  Here());
    }
}
void MultiOMDict::set(const std::string& key, bool val) {
    set(key, (std::int64_t)val);
}
void MultiOMDict::set(const std::string& key, double val) {
    if (multio_grib2_dict_set_double(get(), key.c_str(), val) == 0) {
        throw EncodeMtg2Exception(std::string("Can not set key ") + std::string(key)
                                      + std::string(" with double value ") + std::to_string(val),
                                  Here());
    }
}
void MultiOMDict::set(const std::string& key, const std::int64_t* val, std::size_t len) {
    if (multio_grib2_dict_set_int64_array(get(), key.c_str(), val, len) != 0) {
        throw EncodeMtg2Exception(std::string("Can not set key ") + std::string(key) + std::string(" with int64 array"),
                                  Here());
    }
}
void MultiOMDict::set(const std::string& key, const double* val, std::size_t len) {
    if (multio_grib2_dict_set_double_array(get(), key.c_str(), val, len) != 0) {
        throw EncodeMtg2Exception(
            std::string("Can not set key ") + std::string(key) + std::string(" with double array"), Here());
    }
}
void MultiOMDict::set(const std::string& key, const std::vector<std::int64_t>& val) {
    set(key, val.data(), val.size());
}
void MultiOMDict::set(const std::string& key, const std::vector<double>& val) {
    set(key, val.data(), val.size());
}


void* MultiOMDict::get() {
    return static_cast<void*>(dict_.get());
}


MultiOMEncoder::MultiOMEncoder(MultiOMDict& options) {
    ASSERT(multio_grib2_encoder_open(options.get(), &encoder_) == 0);
}

std::unique_ptr<codes_handle> MultiOMEncoder::encode(MultiOMDict& mars, MultiOMDict& par, const double* data,
                                                     std::size_t len) {
    codes_handle* rawOutputCodesHandle = nullptr;
    ASSERT(multio_grib2_encoder_encode64(encoder_, mars.get(), par.get(), data, len, (void**)&rawOutputCodesHandle)
           == 0);
    return std::unique_ptr<codes_handle>{rawOutputCodesHandle};
}

std::unique_ptr<codes_handle> MultiOMEncoder::encode(MultiOMDict& mars, MultiOMDict& par, const float* data,
                                                     std::size_t len) {
    codes_handle* rawOutputCodesHandle = nullptr;
    ASSERT(multio_grib2_encoder_encode32(encoder_, mars.get(), par.get(), data, len, (void**)&rawOutputCodesHandle)
           == 0);
    return std::unique_ptr<codes_handle>{rawOutputCodesHandle};
}


MultiOMEncoder::~MultiOMEncoder() {
    ASSERT(multio_grib2_encoder_close(&encoder_) == 0);
}


EncodeMtg2Exception::EncodeMtg2Exception(const std::string& r, const eckit::CodeLocation& l) :
    eckit::Exception(encodingExceptionReason(r), l) {}


EncodeMtg2::EncodeMtg2(const ComponentConfiguration& compConf) :
    ChainedAction{compConf}, options_{parseOptions(compConf)}, encoder_{makeEncoder(options_, compConf)} {}


void EncodeMtg2::executeImpl(Message msg) {
    if (msg.tag() != Message::Tag::Field) {
        executeNext(std::move(msg));
        return;
    }

    auto& md = msg.metadata();


    // TODO MIVAL : to be removed
    // std::cout << "Encoding message with metadata: " << md << std::endl;

    // TO encoding
    MultiOMDict mars{MultiOMDictKind::MARS};
    MultiOMDict par{MultiOMDictKind::Parametrization};

    {
        using namespace message;
        // Read and set unscoped mars keys
        auto marsKeys = read(keySet<MarsKeys>().unscoped(), md);
        write(marsKeys, mars);

        // Read scoped misc keys
        auto miscKeys = read(keySet<MiscKeys>().scoped(), md);
        // Write unscoped misc keys
        miscKeys.keySet.unscoped();
        write(miscKeys, par);

        // Handle geometry
        withScopedGeometryKeySet(marsKeys, [&](GridType gridType, auto geoKeySet) {
            MultiOMDict geom{([&]() {
                switch (gridType) {
                    case GridType::GG:
                        return MultiOMDictKind::ReducedGG;
                    case GridType::LL:
                        return MultiOMDictKind::RegularLL;
                    case GridType::SH:
                        return MultiOMDictKind::SH;
                }
                throw EncodeMtg2Exception("unkown gridType", Here());
            })()};

            auto geoKeys = read(geoKeySet, md);
            write(geoKeys.unscoped(), geom);
            par.set_geometry(std::move(geom));
        });

        auto& payload = msg.payload();

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

    // TODO MIVAL : to be removed
    // std::cout << "Exit encoding with metadata: " << md << std::endl;
}

void EncodeMtg2::print(std::ostream& os) const {
    os << "EncodeMtg2(knowledege-root=" << options_.knowledgeRoot.value_or("") << ", ";
    os << "samples-path=" << options_.samplesPath.value_or("") << ", ";
    os << "mapping-rules=" << options_.mappingFile.value_or("") << ", ";
    os << "encoding-rules=" << options_.encodingFile.value_or("");
    os << ")";
}


static ActionBuilder<EncodeMtg2> EncodeMtg2Builder("encode-mtg2");

}  // namespace multio::action::encode_mtg2
