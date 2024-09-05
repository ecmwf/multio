/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Philipp Geier
/// @author Domokos Sármány
/// @author Simon Smart

/// @date Nov 2022

#pragma once

#include <unordered_map>

#include "multio/action/ChainedAction.h"
#include "multio/util/Hash.h"
#include "metkit/mars2grib/Mars2Grib.h"
#include "multio/message/BaseMetadata.h"


namespace multio::action::ifs {

/**
 * Example messag metadata={"format":"raw","precision":"double","missing_value":1.79769e+308,"nmissing":0,"nvalues":654400,"level":1,"repres":1,"prefix":1,"prev_pp":-1,"curr_step":4,"paramID":133","model":"ifsosphere"}
 */

enum class Repres: std::int64_t {
 Gridded=1,
 Spectral=2,
};


struct UniqueFieldId { 
    std::int64_t paramId;
    std::string levtype;
    Repres repres;
    std::int64_t level;
    
    // model; // ifs, wave, ocean ?
    // precision;
};

bool operator==(const UniqueFieldId& lhs, const UniqueFieldId& rhs);
bool operator!=(const UniqueFieldId& lhs, const UniqueFieldId& rhs);

Repres parseRepres(const std::string& str);



enum class TypeOfTimeRange: std::int64_t {
 Instant = 0,
 FixedSize = 1,
 FromLastPP = 2,
 FromBeginning = 3, // Will be selected through a special list
};


struct ParamIdInfo { 
    TypeOfTimeRange typeOfTimeRange;
    std::optional<std::int64_t> typeOfStatisticalProcessing; // Given for non instant
    std::optional<std::int64_t> lengthOfTimeRange;  // Given for FixedSize
    std::optional<std::int64_t> indicatorOfUnitForTimeRange;  // Given for FixedSize
    
    bool emitStepZero; // All Instant - true
                       // All statistical - false
};



using UniqueFieldStepCache = std::unordered_map<UniqueFieldId, std::int64_t>;
using ParamIdInfoCache = std::unordered_map<std::int64_t, ParamIdInfo>;

struct StepRange {
    std::optional<std::int64_t> from;
    std::int64_t current;
};

};


template <>
struct std::hash<multio::action::ifs::UniqueFieldId> {
    std::size_t operator()(const multio::action::ifs::UniqueFieldId& t) const
        noexcept(noexcept(multio::util::hash_combine(t.paramId, t.levtype, t.repres, t.level))) {
        return multio::util::hash_combine(t.paramId, t.levtype, t.repres, t.level);
    }
};



namespace multio::action {

class IfsMapping : public ChainedAction {
public:
    explicit IfsMapping(const ComponentConfiguration& compConf);

    void executeImpl(message::Message msg) override;

protected:
    /**
     * Not all messages from ifs must be processed. 
     * Some fields are accumulated over multiple steps but ifs emits it each step.
     * Sometime the first message (step 0) does not need to be emitted.
     * 
     * @return Null opt if nothing should be send, a step or steprange if the message should be emitted
     */
    std::optional<ifs::StepRange> filterDuplicate(const ifs::UniqueFieldId& id, const ifs::ParamIdInfo& pId, const message::Metadata& md);
    
    const ifs::ParamIdInfo& getParamIdInfo(std::int64_t);
    const ifs::ParamIdInfo& getParamIdInfo(const ifs::UniqueFieldId&);
    ifs::UniqueFieldId fieldIdFromMetadata(const message::Metadata& md);

private:
    void print(std::ostream& os) const override;

    ifs::ParamIdInfoCache paramIdInfo_;
    ifs::UniqueFieldStepCache stepsPerField_;
    
    eckit::ValueMap m2gPid_;
    eckit::ValueMap m2gPrefixToLevType_;
    message::BaseMetadata levTypeMd_;
};

}  // namespace multio::action
