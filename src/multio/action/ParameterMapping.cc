#include "ParameterMapping.h"

namespace multio {
namespace action {

namespace {
std::string getMappingName(
    const ConfigurationContext& confCtx) {
    if (!confCtx.config().has("mapping")) {
        throw eckit::Exception(
            "An action of type \"parameter-mapping\" needs to have a field \"mapping\".");
    }
    return confCtx.config().getString("mapping");
}
}  // namespace

ParameterMapping::ParameterMapping(const ConfigurationContext& confCtx) :
    Action(confCtx), name_(getMappingName(confCtx)), mappings_(confCtx.parameterMappings().getMappings(name_)), options_{} {
    options_.enforceMatch = confCtx.config().getBool("enforce-match", true);
    options_.overwriteExisting = confCtx.config().getBool("overwrite-existing", false);
};

void ParameterMapping::executeImpl(message::Message msg) const {
    executeNext(msg.modifyMetadata(apply(std::move(msg).metadata())));
};

void ParameterMapping::applyInplace(message::Metadata& md) const {
    for (const auto& m : mappings_) {
        m.applyInplace(md, options_);
    }
};
message::Metadata ParameterMapping::apply(const message::Metadata& md) const {
    message::Metadata mdc(md);
    applyInplace(mdc);
    return mdc;
};
message::Metadata ParameterMapping::apply(message::Metadata&& md) const {
    message::Metadata mdc(std::move(md));
    applyInplace(mdc);
    return mdc;
};

void ParameterMapping::print(std::ostream& os) const {
    os << "ParameterMapping(mapping=" << (name_)
       << ", enforce-match=" << (options_.enforceMatch ? "true" : "false")
       << ", overwrite-existing=" << (options_.overwriteExisting ? "true" : "false") << ")";
}


static ActionBuilder<ParameterMapping> AggregationBuilder("parameter-mapping");
}  // namespace action
}  // namespace multio
