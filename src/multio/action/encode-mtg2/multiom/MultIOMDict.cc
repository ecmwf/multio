/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "MultIOMDict.h"

#include "eckit/log/Log.h"

#include "multio/action/encode-mtg2/EncodeMtg2Exception.h"
#include "multiom/api/c/api.h"

namespace multio::action {


std::string multIOMDictKindString(MultIOMDictKind kind) {
    switch (kind) {
        case MultIOMDictKind::Options:
            return "options";
        case MultIOMDictKind::MARS:
            return "mars";
        case MultIOMDictKind::Parametrization:
            return "parametrization";
        case MultIOMDictKind::ReducedGG:
            return "reduced-gg";
        case MultIOMDictKind::RegularLL:
            return "regular-ll";
        case MultIOMDictKind::SH:
            return "sh";
        case MultIOMDictKind::HEALPix:
            return "HEALPix";
        default:
            NOTIMP;
    }
}


MultIOMDict::MultIOMDict(MultIOMDictKind kind) : kind_{kind} {
    std::string kindStr = multIOMDictKindString(kind);
    void* dict = NULL;
    if (multio_grib2_dict_create(&dict, kindStr.data()) != 0) {
        throw EncodeMtg2Exception(std::string("Can not create dict kind ") + kindStr, Here());
    }

    if (kind == MultIOMDictKind::Options) {
        ASSERT(multio_grib2_init_options(&dict) == 0);
    }
    dict_.reset(static_cast<ForeignDictType*>(dict));
}

void MultIOMDict::toYAML(const std::string& file) {
    multio_grib2_dict_to_yaml(get(), "stdout");
}

void MultIOMDict::set(const char* key, const char* val) {
    if (multio_grib2_dict_set(get(), key, val) != 0) {
        throw EncodeMtg2Exception(
            std::string("Can not set key ") + std::string(key) + std::string(" with value ") + std::string(val),
            Here());
    }
}

void MultIOMDict::set(const std::string& key, const std::string& val) {
    set(key.c_str(), val.c_str());
}

void MultIOMDict::set_geometry(MultIOMDict&& geom) {
    ASSERT(kind_ == MultIOMDictKind::Parametrization);
    switch (geom.kind_) {
        case MultIOMDictKind::HEALPix:
        case MultIOMDictKind::ReducedGG:
        case MultIOMDictKind::RegularLL:
        case MultIOMDictKind::SH:
            geom_ = std::make_unique<MultIOMDict>(std::move(geom));
            ASSERT(multio_grib2_dict_set_geometry(get(), geom_->get()) == 0);
            break;
        default:
            throw EncodeMtg2Exception("Passed dict is not a geometry dict", Here());
    }
}


void MultIOMDict::set(const std::string& key, std::int64_t val) {
    if (multio_grib2_dict_set_int64(get(), key.c_str(), val) != 0) {
        throw EncodeMtg2Exception(std::string("Can not set key ") + std::string(key) + std::string(" with int64 value ")
                                      + std::to_string(val),
                                  Here());
    }
}
void MultIOMDict::set(const std::string& key, bool val) {
    set(key, (std::int64_t)val);
}
void MultIOMDict::set(const std::string& key, double val) {
    if (multio_grib2_dict_set_double(get(), key.c_str(), val) != 0) {
        throw EncodeMtg2Exception(std::string("Can not set key ") + std::string(key)
                                      + std::string(" with double value ") + std::to_string(val),
                                  Here());
    }
}
void MultIOMDict::set(const std::string& key, const std::int64_t* val, std::size_t len) {
    if (multio_grib2_dict_set_int64_array(get(), key.c_str(), val, len) != 0) {
        throw EncodeMtg2Exception(std::string("Can not set key ") + std::string(key) + std::string(" with int64 array"),
                                  Here());
    }
}
void MultIOMDict::set(const std::string& key, const double* val, std::size_t len) {
    if (multio_grib2_dict_set_double_array(get(), key.c_str(), val, len) != 0) {
        throw EncodeMtg2Exception(
            std::string("Can not set key ") + std::string(key) + std::string(" with double array"), Here());
    }
}
void MultIOMDict::set(const std::string& key, const std::vector<std::int64_t>& val) {
    set(key, val.data(), val.size());
}
void MultIOMDict::set(const std::string& key, const std::vector<double>& val) {
    set(key, val.data(), val.size());
}


void* MultIOMDict::get() const {
    return static_cast<void*>(dict_.get());
}

std::string MultIOMDict::toJSON() const {
    char* d;
    if (multio_grib2_dict_to_json(get(), &d) != 0) {
        throw EncodeMtg2Exception("Can not export to json ", Here());
    }
    return std::string(d);
}


MultIOMDict MultIOMDict::makeOptions(const EncodeMtg2Conf& opts) {
    MultIOMDict optDict(MultIOMDictKind::Options);

    using namespace datamod;

    // Select a subset of the options (excluding knowledge-root) and set them to the opt dict
    write(read(keySet<EncodeMtg2Def::SamplesPath, EncodeMtg2Def::MappingRules, EncodeMtg2Def::EncodingRules>(), opts),
          optDict);

    // TODO -- this hack will just be removed through
    const auto& knowledgeRoot = key<EncodeMtg2Def::KnowledgeRoot>(opts);
    if (!knowledgeRoot.isMissing()) {
        setenv("IFS_INSTALL_DIR", knowledgeRoot.get().c_str(), 0);
    }

    // TODO set codes sample path...

    return optDict;
}


}  // namespace multio::action
