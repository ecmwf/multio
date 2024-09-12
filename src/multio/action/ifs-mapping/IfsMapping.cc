#include "IfsMapping.h"
#include "multio/message/BaseMetadata.h"
#include "multio/message/Metadata.h"
#include "multio/message/Glossary.h"
#include "multio/util/Mars2GribMetadataSetter.h"
#include "multio/util/Grib2TimeUnit.h"
#include "metkit/mars2grib/Mars2Grib.h"

#include <utility>

namespace multio::action {

namespace ifs {

bool operator==(const UniqueFieldId& lhs, const UniqueFieldId& rhs) {
    return (lhs.paramId == rhs.paramId) && (lhs.levtype == rhs.levtype) && (lhs.repres == rhs.repres) && (lhs.level == rhs.level);
};
bool operator!=(const UniqueFieldId& lhs, const UniqueFieldId& rhs) {
    return !(lhs == rhs); 
};


Repres parseRepres(const std::string& str) {
    if(str.rfind("gridded", 0) == 0)  {
        return Repres::Gridded;
    }
    if(str.rfind("spherical_harmonics", 0) == 0)  {
        return Repres::Spectral;
    }
    NOTIMP;
}



}

using message::glossary;


IfsMapping::IfsMapping(const ComponentConfiguration& compConf) :
    ChainedAction(compConf) {
};

void IfsMapping::executeImpl(message::Message msg) {
    switch (msg.tag()) {
        case (message::Message::Tag::Field): {
            const auto& uFid = fieldIdFromMetadata(msg.metadata());
            const auto& pIdInfo = getParamIdInfo(uFid);
            
            auto stepRange = filterDuplicate(uFid, pIdInfo, msg.metadata());
            
            if (stepRange) {
                msg.acquireMetadata();
                auto& md = msg.modifyMetadata();
                if (stepRange->from) {
                    md.set(glossary().startStep, *stepRange->from);
                }
                md.set(glossary().endStep, stepRange->current);
                md.set(glossary().step, stepRange->current);
                executeNext(std::move(msg));
            }
            
            break;
        }
        default: {
            executeNext(std::move(msg));
            break;
        }
    };
};


std::optional<ifs::StepRange> IfsMapping::filterDuplicate(const ifs::UniqueFieldId& id, const ifs::ParamIdInfo& pIdInfo, const message::Metadata& md) {
    std::int64_t currStep = md.get<std::int64_t>(glossary().ifsCurrStep);
    // Future
    // std::int64_t prevPP = md.get<std::int64_t>(glossary().ifsPrevPP);
    
    std::optional<ifs::StepRange> ret;
    
    auto search = stepsPerField_.find(id);
    switch (pIdInfo.typeOfTimeRange) {
        case ifs::TypeOfTimeRange::Instant: {
            // ASSERT(prevPP == -1); // future
            if (currStep == 0) {
                ASSERT(search == stepsPerField_.end());
                
                if (pIdInfo.emitStepZero)  {
                    ret = {{}, currStep};
                } 
                stepsPerField_.insert_or_assign(id, 0);
            } else {
                ASSERT(search != stepsPerField_.end());
                ASSERT(search->second != currStep);
                search->second = currStep;
                ret = {{}, currStep};
            }
        }
        break;
        case ifs::TypeOfTimeRange::FixedSize: {
            // ASSERT(prevPP == -1); // future
            ASSERT(pIdInfo.typeOfStatisticalProcessing);
            ASSERT(pIdInfo.lengthOfTimeRange);
            ASSERT(pIdInfo.indicatorOfUnitForTimeRange);
            
            if (currStep == 0) {
                ASSERT(search == stepsPerField_.end());
                
                // TODO - should not happen?
                if (pIdInfo.emitStepZero)  {
                    ret = {{currStep}, currStep};
                } 
                stepsPerField_.insert_or_assign(id, 0);
            } else {
                ASSERT(search != stepsPerField_.end());
                
                // Need to last processing step to 0 - then window computation will be correct.
                // I.e. for 6h the next step will be computed to be 0 + 6 = 6
                // the step after 12, the step after 18 ...
                auto nextPP = search->second + (util::grib2TimeUnitToHours(util::Grib2TimeUnit{*pIdInfo.indicatorOfUnitForTimeRange}) * (*pIdInfo.lengthOfTimeRange));
                if (currStep >= nextPP) {
                    ret = {{search->second}, currStep};
                    search->second = currStep;
                }
            }
            
        }
        break;
        case ifs::TypeOfTimeRange::FromLastPP: {
            ASSERT(pIdInfo.typeOfStatisticalProcessing);
            // Currently same as instant... until prev_pp emits different information and we can reconstruct something useful
            
            if (currStep == 0) {
                ASSERT(search == stepsPerField_.end());
                
                // TODO - should not happen?
                if (pIdInfo.emitStepZero)  {
                    ret = {{currStep}, currStep};
                } 
                stepsPerField_.insert_or_assign(id, 0);
            } else {
                ASSERT(search != stepsPerField_.end());
                ASSERT(search->second != currStep);
                
                ret = {{search->second}, currStep};
                search->second = currStep;
            }
        }
        break;
        case ifs::TypeOfTimeRange::FromBeginning: {
            if (currStep == 0) {
                ASSERT(search == stepsPerField_.end());
                
                // TODO - should not happen?
                if (pIdInfo.emitStepZero)  {
                    ret = {{currStep}, currStep};
                } 
                stepsPerField_.insert_or_assign(id, 0);
            } else {
                ASSERT(search != stepsPerField_.end());
                ASSERT(search->second != currStep);
                
                ret = {{0}, currStep};
                search->second = currStep;
            }
        }
        break;
    }
    
    return ret;
}


ifs::UniqueFieldId IfsMapping::fieldIdFromMetadata(const message::Metadata& md) {
    ifs::UniqueFieldId res;

    res.paramId = md.get<std::int64_t>(glossary().paramId);
    res.level = md.get<std::int64_t>(glossary().level);

    md.get(glossary().ifsPrefix).visit([&](const auto& val) -> void {
        using T = std::decay_t<decltype(val)>;
        if constexpr (std::is_same_v<T, std::string>) {
            m2gPrefixToLevType_.insert_or_assign(eckit::Value{glossary().ifsPrefix}, eckit::Value{val});
            return;
        } 
        if constexpr (std::is_same_v<T, std::int64_t>) {
            m2gPrefixToLevType_.insert_or_assign(eckit::Value{glossary().ifsPrefix}, eckit::Value{val});
            return;
        }
        NOTIMP; 
    }); 
    
    m2gPrefixToLevType_.insert_or_assign(eckit::Value{glossary().paramId}, eckit::Value{res.paramId});
    m2gPrefixToLevType_.insert_or_assign(eckit::Value{glossary().level}, eckit::Value{res.level});
    
    
    util::Mars2GribMetadataSetter keySetter{levTypeMd_, true};
    metkit::mars2grib::convertMars2Grib(m2gPrefixToLevType_, keySetter, metkit::mars2grib::ifsPrefixToLevTypeRuleList());
    res.levtype = levTypeMd_.get<std::string>("levType");
    
    
    res.repres = md.get(glossary().ifsRepres).visit([](const auto& val) -> ifs::Repres {
        using T = std::decay_t<decltype(val)>;
        if constexpr (std::is_same_v<T, std::string>) {
            return ifs::parseRepres(val);
        }
        if constexpr (std::is_same_v<T, std::int64_t>) {
            return ifs::Repres{val};
        }
        NOTIMP; 
    }); 
    
    return res;
}

const ifs::ParamIdInfo& IfsMapping::getParamIdInfo(std::int64_t pid) {
    auto searchPid = paramIdInfo_.find(pid);
    
    if (searchPid == paramIdInfo_.end()) {
        m2gPid_.insert_or_assign("paramId", pid);
        message::BaseMetadata md;
        util::Mars2GribMetadataSetter keySetter{md, true};
        metkit::mars2grib::convertMars2Grib(m2gPid_, keySetter, metkit::mars2grib::statParamRuleList());
        
        ifs::ParamIdInfo info;
        if (auto tosp = md.getOpt<std::int64_t>("typeOfStatisticalProcessing"); tosp) {
            info.typeOfStatisticalProcessing = *tosp;
            info.typeOfTimeRange = ifs::TypeOfTimeRange::FromLastPP;
            info.emitStepZero = false;
            
            if (auto len = md.getOpt<std::int64_t>("lengthOfTimeRange"); len) {
                info.lengthOfTimeRange = *len;
                info.typeOfTimeRange = ifs::TypeOfTimeRange::FixedSize;
            }
            if (auto ind = md.getOpt<std::int64_t>("indicatorOfUnitForTimeRange"); ind) {
                info.indicatorOfUnitForTimeRange = ind;
            }
        } else {
            info.typeOfTimeRange = ifs::TypeOfTimeRange::Instant;
            info.emitStepZero = true;
        }
        
        // TODO handle TypeOfTimeRange::FromBeginning through config ? or maybe throug mars mapping if possible
        
        return paramIdInfo_.emplace(pid, info).first->second;
    } else {
        return searchPid->second;
    }
}

const ifs::ParamIdInfo& IfsMapping::getParamIdInfo(const ifs::UniqueFieldId& fid) {
    return getParamIdInfo(fid.paramId);
}



void IfsMapping::print(std::ostream& os) const {
    os << "IfsMapping()";
}


static ActionBuilder<IfsMapping> IfsMappingBuilder("ifs-mapping");

}  // namespace multio::action
