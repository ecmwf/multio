#include "Mapping.h"

#include <string>
#include <cstdlib>

#include "multio/LibMultio.h"
#include "multio/message/Glossary.h"
#include "multio/util/Substitution.h"


namespace multio::action {

using message::glossary;

ScaleMapping::ScaleMapping( const config::ComponentConfiguration& compConf ):
hasMapping_(false),scaleMap_{} {

    const auto mappings
        = compConf.parsedConfig().has("mapping-definition") ? compConf.parsedConfig().getSubConfigurations("mapping-definition") : std::vector<eckit::LocalConfiguration>{};

    if (!mappings.empty()) {
        hasMapping_ = true;
        for (const auto& mapping : mappings) {
            auto matcher = mapping.getSubConfiguration("case");
            scaleMap_[matcher.getString("param-is")] = matcher.getString("map-to-param");
        }
    }

}

void ScaleMapping::applyMapping( message::Metadata& md ) const {
    if ( hasMapping_ ) {
        std::string cparam{"xxx"};
        if (auto param = md.getOpt<std::string>(glossary().param); param) {
            cparam = *param;
        }
        if (auto paramId = md.getOpt<std::int64_t>(glossary().paramId); paramId) {
            cparam = std::to_string( *paramId );
        }
        else {
            throw eckit::SeriousBug{"param/paramId metadata not present", Here()};
        }
        
        auto it = scaleMap_.find(cparam);
        if ( it != scaleMap_.end() ) {
            md.set(glossary().paramId, std::int64_t(::atol(it->second.c_str())));
            md.set(glossary().param, it->second.c_str() );
        }
    }
}

}