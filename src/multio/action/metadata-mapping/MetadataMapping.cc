#include "MetadataMapping.h"

namespace multio::action {

namespace {
std::string getMappingName(const ComponentConfiguration& compConf) {
    if (!compConf.YAML().has("mapping")) {
        throw message::MetadataMappingException(
            "An action of type \"metadata-mapping\" needs to have a field \"mapping\" pointing to a YAML file..",
            Here());
    }
    return compConf.YAML().getString("mapping");
}
}  // namespace

MetadataMapping::MetadataMapping(const ComponentConfiguration& compConf) :
    ChainedAction(compConf),
    name_(getMappingName(compConf)),
    mappings_(compConf.multioConfig().getMetadataMappings(name_)),
    options_{} {
    options_.enforceMatch = compConf.YAML().getBool("enforce-match", true);
    options_.overwriteExisting = compConf.YAML().getBool("overwrite-existing", false);
};

void MetadataMapping::executeImpl(message::Message msg) {
    switch (msg.tag()) {
        case (message::Message::Tag::Field): {
            executeNext(msg.modifyMetadata(apply(std::move(msg).metadata())));
            break;
        }
        default: {
            executeNext(msg);
            break;
        }
    };
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
    os << "MetadataMapping(mapping=" << (name_) << ", enforce-match=" << (options_.enforceMatch ? "true" : "false")
       << ", overwrite-existing=" << (options_.overwriteExisting ? "true" : "false") << ")";
}


static ActionBuilder<MetadataMapping> MetadataMappingBuilder("metadata-mapping");
}  // namespace multio::action
