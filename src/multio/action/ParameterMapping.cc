#include "ParameterMapping.h"

namespace multio {
namespace action {

namespace {
std::pair<std::string, std::shared_ptr<std::vector<message::ParameterMapping>>> getMappings(
    const ConfigurationContext& confCtx) {
    if (!confCtx.config().has("mapping")) {
        throw eckit::Exception(
            "An action of type \"parameter-mapping\" needs to have a field \"mapping\".");
    }
    return confCtx.parameterMappings().getMappings(confCtx.config().getString("mapping"));
}
}  // namespace

ParameterMapping::ParameterMapping(const ConfigurationContext& confCtx) :
    Action(confCtx), mappings_(getMappings(confCtx)), enforceMatch_(confCtx.config().getBool("enforce-match", false)) {};

void ParameterMapping::execute(message::Message msg) const {
    executeNext(msg.modifyMetadata(apply(std::move(msg).metadata())));
};

void ParameterMapping::applyInplace(message::Metadata& md) const {
    for (const auto& m : *(mappings_.second)) {
        m.applyInplace(md, enforceMatch_);
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
    os << "ParameterMapping(mapping=" << (mappings_.first)
       << ", enforce-match=" << (enforceMatch_ ? "true" : "false") << ")";
}


static ActionBuilder<ParameterMapping> AggregationBuilder("parameter-mapping");
}  // namespace action
}  // namespace multio
