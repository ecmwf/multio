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
#include "multio/config/PathConfiguration.h"
#include "multio/message/Glossary.h"
#include "multio/message/Parametrization.h"
#include "multio/util/MioGribHandle.h"
#include "multio/util/PrecisionTag.h"

namespace multio::action::encode_mtg2 {

using config::configuration_path_name;

using message::glossary;
using message::Message;
using message::MetadataTypes;
using message::Parametrization;
using message::Peer;


namespace {

EncodeMultiOMOptions parseOptions(const ComponentConfiguration& compConf) {
    EncodeMultiOMOptions ret;
    const auto& conf = compConf.parsedConfig();

    if (conf.has("knowledge-root") && !conf.isNull("knowledge-root")) {
        ret.knowledgeRoot = conf.getString("knowledge-root");
    }
    else {
        ret.knowledgeRoot = multio::LibMultio::instance().libraryHome();
    }

    if (conf.has("samples-path") && !conf.isNull("samples-path")) {
        ret.samplesPath = conf.getString("samples_path");
    }
    else {
        ret.samplesPath = ret.knowledgeRoot.value() + "/share/multiom/samples";
    }

    if (conf.has("mapping-rules") && !conf.isNull("mapping-rules")) {
        ret.mappingFile = conf.getString("mapping-rules");
    }
    else {
        ret.mappingFile = ret.knowledgeRoot.value() + "/share/multiom/mappings/mapping-rules.yaml";
    }

    if (conf.has("encoding-rules") && !conf.isNull("encoding-rules")) {
        ret.encodingFile = conf.getString("encoding-rules");
    }
    else {
        ret.encodingFile = ret.knowledgeRoot.value() + "/share/multiom/encodings/encoding-rules.yaml";
    }


    if (conf.has("geo-from-atlas")) {
        ret.geoFromAtlas = conf.getBool("geo-from-atlas");
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

    // TODO -- this hack will just be removed through
    if (opts.knowledgeRoot) {
        setenv("IFS_INSTALL_DIR", opts.knowledgeRoot->c_str(), 0);
    }

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
    if (multio_grib2_dict_set_double(get(), key.c_str(), val) != 0) {
        throw EncodeMtg2Exception(std::string("Can not set key ")
                                      + std::string(key)
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
        using namespace message::Mtg2;
        withMarsKeys([&](const auto& kvDescr) {
            if (auto search = md.find(kvDescr); search != md.end()) {
                mars.set(kvDescr, kvDescr.get(search->second));
            }
            else {
                // Hack for defaults until properly implemented in different branch
                if (kvDescr.key == mars::origin.key) {
                    mars.set(kvDescr, std::string("ecmf"));
                }
            }
        });
        withParametrizationKeys([&](const auto& prefixedKvDescr) {
            if (auto search = md.find(prefixedKvDescr.prefixed); search != md.end()) {
                par.set(prefixedKvDescr.plain, prefixedKvDescr.prefixed.get(search->second));
            }
            else {
                // Hack for defaults until properly implemented in different branch
                if (prefixedKvDescr.plain.key == misc::initialStep.plain.key) {
                    par.set(prefixedKvDescr.plain, (std::int64_t)0);
                }
                if (prefixedKvDescr.plain.key == misc::lengthOfTimeStepInSeconds.plain.key) {
                    par.set(prefixedKvDescr.plain, (std::int64_t)3600);
                }
            }
        });

        // Legacy handling -- assume no grid has been passed for SH but truncation and repres are given
        std::string grid;
        auto searchGrid = md.find(mars::grid);
        if (searchGrid == md.end()) {
            auto searchRepres = md.find(marsLegacy::repres);
            auto searchTruncation = md.find(marsLegacy::truncation);

            if (searchRepres != md.end() && searchTruncation != md.end()) {
                if (searchRepres->second.get<std::string>() == "sh") {
                    grid = std::string("TCO") + std::to_string(searchTruncation->second.get<std::int64_t>());
                }
                else {
                    throw EncodeMtg2Exception("Required a key \"grid\"", Here());
                }
            }
            else {
                throw EncodeMtg2Exception("Required a key \"grid\"", Here());
            }
        }
        else {
            grid = mars::grid.get(searchGrid->second);
        }

        {
            Repres repres;
            std::string prefix;
            std::tie(repres, prefix) = represAndPrefixFromGridName(grid);

            // Legacy handling -- add repres and truncation assuming grid has been passed properly and also handles SH
            // {
            //     if (auto searchRepres = md.find(marsLegacy::repres); searchRepres != md.end()) {
            //         if (searchRepres->second.get<std::string>() != toString(repres)) {
            //             std::ostringstream oss;
            //             oss << "Passed repres \"" << searchRepres->second << "\" is different from infered repres \""
            //             << toString(repres) << "\""; throw EncodeMtg2Exception(oss.str(), Here());
            //         }
            //     }
            //     mars.set(marsLegacy::repres, toString(repres));
            //
            //     if (repres == Repres::SH) {
            //         std::int64_t truncation = std::stol(grid.substr(3, grid.size()-1));
            //         if (auto searchTruncation = md.find(marsLegacy::truncation); searchTruncation != md.end()) {
            //             if (searchTruncation->second.get<std::int64_t>() != truncation) {
            //                 std::ostringstream oss;
            //                 oss << "Passed truncation \"" << searchTruncation->second << "\" is different from
            //                 infered truncation \"" << truncation << "\""; throw EncodeMtg2Exception(oss.str(),
            //                 Here());
            //             }
            //         }
            //         mars.set(marsLegacy::truncation, truncation);
            //     }
            //
            // }
            //
            MultiOMDict geom{([&]() {
                switch (repres) {
                    case Repres::GG:
                        return MultiOMDictKind::ReducedGG;
                    case Repres::LL:
                        return MultiOMDictKind::RegularLL;
                    case Repres::SH:
                        return MultiOMDictKind::SH;
                }
                throw EncodeMtg2Exception("unkown repres", Here());
            })()};

            withGeometryKeys(repres, [&](const auto& kvDescr) {
                const auto& global = Parametrization::instance().get();
                if (options_.geoFromAtlas && (global.find(prefix) == global.end())) {
                    extract::AtlasGeoSetter::handleGrid(prefix, grid);
                }

                if (auto search = md.find(prefix + std::string(kvDescr)); search != md.end()) {
                    geom.set(kvDescr, kvDescr.get(search->second));
                }
            });

            par.set_geometry(std::move(geom));
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

            // TODO MIVAL : to be removed
            // std::cout << "Encoded message size: " << buf.size() << std::endl;

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
    os << "encoding-rules=" << options_.encodingFile.value_or("") << ", ";
    os << "geo-from-atlas=" << (options_.geoFromAtlas ? "true" : "false");
    os << ")";
}


static ActionBuilder<EncodeMtg2> EncodeMtg2Builder("encode-mtg2");

}  // namespace multio::action::encode_mtg2
