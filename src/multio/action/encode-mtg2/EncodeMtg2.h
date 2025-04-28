/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany
/// @author Simon Smart
/// @author Tiago Quintino

/// @date Oct 2019

#pragma once


#include <memory>
#include <optional>
#include <string>

#include "eccodes.h"
#include "metkit/codes/CodesHandleDeleter.h"
#include "multio/action/ChainedAction.h"
#include "multiom/api/c/api.h"

namespace multio::action::encode_mtg2 {

enum class MultiOMDictKind : unsigned long
{
    Options,
    MARS,
    Parametrization,
    // Geometry dicts
    ReducedGG,
    RegularLL,
    SH,
};

struct EncodeMultiOMOptions {
    std::optional<std::string> knowledgeRoot;
    std::optional<std::string> samplesPath;
    std::optional<std::string> encodingFile;
    std::optional<std::string> mappingFile;
};

std::string multiOMDictKindString(MultiOMDictKind kind);

struct MultiOMDict {
    MultiOMDict(MultiOMDictKind kind);

    void toYAML(const std::string& file = "stdout");

    void set(const char* key, const char* val);
    void set(const std::string& key, const std::string& val);

    // Typed setters
    void set(const std::string& key, std::int64_t val);
    void set(const std::string& key, double val);
    void set(const std::string& key, bool val);
    void set(const std::string& key, const std::int64_t* val, std::size_t len);
    void set(const std::string& key, const double* val, std::size_t len);
    void set(const std::string& key, const std::vector<std::int64_t>& val);
    void set(const std::string& key, const std::vector<double>& val);

    // Set geoemtry on parametrization
    void set_geometry(MultiOMDict& geom);

    ~MultiOMDict();

    void* get();

    MultiOMDictKind kind_;
    void* dict_ = nullptr;
};


struct MultiOMEncoder {
    MultiOMEncoder(MultiOMDict& options);

    std::unique_ptr<codes_handle> encode(MultiOMDict& mars, MultiOMDict& par, const double* data, std::size_t len);
    std::unique_ptr<codes_handle> encode(MultiOMDict& mars, MultiOMDict& par, const float* data, std::size_t len);

    ~MultiOMEncoder();

    void* encoder_ = nullptr;
};


class EncodeMtg2 : public ChainedAction {
public:
    explicit EncodeMtg2(const ComponentConfiguration& compConf);

    void executeImpl(message::Message msg) override;

private:
    // Internal constructor delegate with prepared configuration for specific
    // encoder
    explicit EncodeMtg2(const ComponentConfiguration& compConf, const eckit::LocalConfiguration& encoderConf);

    void print(std::ostream& os) const override;

    EncodeMultiOMOptions options_;
    MultiOMEncoder encoder_;
};

//---------------------------------------------------------------------------------------------------------------------

class EncodeMtg2Exception : public eckit::Exception {
public:
    EncodeMtg2Exception(const std::string& reason, const eckit::CodeLocation& location = eckit::CodeLocation());
};

//---------------------------------------------------------------------------------------------------------------------


}  // namespace multio::action::encode_mtg2
