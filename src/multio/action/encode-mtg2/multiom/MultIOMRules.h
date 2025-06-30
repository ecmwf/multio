/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/// @author Philipp Geier

/// @date Oct 2025

#pragma once

#include "eckit/config/LocalConfiguration.h"
#include "multio/action/encode-mtg2/EncoderConf.h"
#include "multio/action/encode-mtg2/Options.h"
#include "multio/action/encode-mtg2/multiom/MultIOMDict.h"
#include "multio/action/encode/GribEncoder.h"
#include "multio/config/ComponentConfiguration.h"
#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/DataModelling.h"
#include "multio/util/MioGribHandle.h"

#include <memory>

#include "multiom/api/c/api.h"

// explicit interface to the C API for rules management
extern "C" {
int multio_grib2_rules_open(void* options, void** handle, const char* fname, int len);
int multio_grib2_rules_close(void** handle);
int multio_grib2_rules_search(void* handle, void* mars_dict, char** rule_name);
// int multio_grib2_rules_print(void* handle, const char* output_file, int len);
// int multio_grib2_rules_size(void* handle, int64_t* num_rules, int64_t max_linear_size, int64_t* max_levels);
}


namespace multio::action {
struct ForeignRulesType;
}

template <>
class std::default_delete<multio::action::ForeignRulesType> {
public:
    void operator()(multio::action::ForeignRulesType* ptr) const {
        void* p = static_cast<void*>(ptr);
        ASSERT(multio_grib2_rules_close(&p) == 0);
    }
};


namespace multio::action {


// New encoder for caching
struct MultIOMRules {
    MultIOMRules(const MultIOMDict& options, const std::string& fname);
    MultIOMRules(const MultIOMDict& options, const EncodeMtg2Conf& conf);
    ~MultIOMRules() = default;

    MultIOMRules(MultIOMRules&&) noexcept = default;
    MultIOMRules& operator=(MultIOMRules&&) noexcept = default;

    EncoderInfo search(const MultIOMDict& mars);

    void* get() const;

    std::unique_ptr<ForeignRulesType> rules_;
};


//---------------------------------------------------------------------------------------------------------------------


}  // namespace multio::action
