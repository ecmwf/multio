#include "MetadataMapping.h"

namespace multio {
namespace action {

namespace {
std::string getMappingName(
    const ConfigurationContext& confCtx) {
    if (!confCtx.config().has("mapping")) {
        throw eckit::Exception(
            "An action of type \"metadata-mapping\" needs to have a field \"mapping\" pointing to a YAML file..");
    }
    return confCtx.config().getString("mapping");
}
}  // namespace

MetadataMapping::MetadataMapping(const ConfigurationContext& confCtx) :
    ChainedAction(confCtx), name_(getMappingName(confCtx)), mappings_(confCtx.metadataMappings().getMappings(name_)), options_{} {
    options_.enforceMatch = confCtx.config().getBool("enforce-match", true);
    options_.overwriteExisting = confCtx.config().getBool("overwrite-existing", false);
};

void MetadataMapping::executeImpl(message::Message msg) const {
    executeNext(msg.modifyMetadata(apply(std::move(msg).metadata())));
};

void MetadataMapping::applyInplace(message::Metadata& md) const {
    for (const auto& m : mappings_) {
        m.applyInplace(md, options_);
    }
};
message::Metadata MetadataMapping::apply(const message::Metadata& md) const {
    message::Metadata mdc(md);
    applyInplace(mdc);
    return mdc;
};
message::Metadata MetadataMapping::apply(message::Metadata&& md) const {
    message::Metadata mdc(std::move(md));
    applyInplace(mdc);
    return mdc;
};

void MetadataMapping::print(std::ostream& os) const {
    os << "MetadataMapping(mapping=" << (name_)
       << ", enforce-match=" << (options_.enforceMatch ? "true" : "false")
       << ", overwrite-existing=" << (options_.overwriteExisting ? "true" : "false") << ")";
}


static ActionBuilder<MetadataMapping> AggregationBuilder("metadata-mapping");
}  // namespace action
}  // namespace multio
