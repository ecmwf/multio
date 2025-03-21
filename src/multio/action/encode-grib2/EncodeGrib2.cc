/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "EncodeGrib2.h"

#include <iostream>

#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"

#include "multio/LibMultio.h"
#include "multio/config/PathConfiguration.h"

namespace multio::action {

using config::configuration_path_name;

using multio::message::Message;

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
    std::string s("Grib2 Enocding exception: ");
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
        case MultiOMDictKind::Geometry:
            return "geometry";
        default:
            NOTIMP;
    }
}

MultiOMDict::MultiOMDict(MultiOMDictKind kind) {
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

MultiOMDict::~MultiOMDict() {
    ASSERT(multio_grib2_dict_destroy(&dict_) == 0);
}

void* MultiOMDict::get() {
    return dict_;
}


MultiOMEncoder::MultiOMEncoder(MultiOMDict& options) {
    ASSERT(multio_grib2_encoder_open(options.get(), &encoder_) == 0);
}

std::unique_ptr<codes_handle> MultiOMEncoder::encode(MultiOMDict& mars, MultiOMDict& par, double* data,
                                                     std::size_t len) {
    codes_handle* rawOutputCodesHandle = nullptr;
    ASSERT(multio_grib2_encoder_encode64(encoder_, mars.get(), par.get(), data, len, (void**)&rawOutputCodesHandle));
    return std::unique_ptr<codes_handle>{rawOutputCodesHandle};
}


MultiOMEncoder::~MultiOMEncoder() {
    ASSERT(multio_grib2_encoder_close(&encoder_) == 0);
}


EncodeGrib2Exception::EncodeGrib2Exception(const std::string& r, const eckit::CodeLocation& l) :
    eckit::Exception(encodingExceptionReason(r), l) {}


EncodeGrib2::EncodeGrib2(const ComponentConfiguration& compConf) :
    ChainedAction{compConf}, options_{parseOptions(compConf)}, encoder_{makeEncoder(options_, compConf)} {}

void EncodeGrib2::executeImpl(Message msg) {
    if (msg.tag() != Message::Tag::Field) {
        executeNext(std::move(msg));
        return;
    }

    // TO encoding

    // executeNext(encodeField(std::move(msg), gridUID));
}

void EncodeGrib2::print(std::ostream& os) const {
    os << "EncodeGrib2(knowledege-root=" << options_.knowledgeRoot.value_or("") << ", ";
    os << "samples-path=" << options_.samplesPath.value_or("") << ", ";
    os << "mapping-rules=" << options_.mappingFile.value_or("") << ", ";
    os << "encoding-rules=" << options_.encodingFile.value_or("");
    os << ")";
}


static ActionBuilder<EncodeGrib2> EncodeGrib2Builder("encode-grib2");

}  // namespace multio::action
