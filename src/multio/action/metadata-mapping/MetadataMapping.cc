#include "MetadataMapping.h"

namespace multio::action::metadata_mapping {

namespace {
std::string getMappingName(const ComponentConfiguration& compConf) {
    if (!compConf.parsedConfig().has("mapping")) {
        throw message::MetadataMappingException(
            "An action of type \"metadata-mapping\" needs to have a field \"mapping\" pointing to a YAML file..",
            Here());
    }
    return compConf.parsedConfig().getString("mapping");
}
}  // namespace

MetadataMapping::MetadataMapping(const ComponentConfiguration& compConf) :
    ChainedAction(compConf),
    name_(getMappingName(compConf)),
    mappings_(compConf.multioConfig().getMetadataMappings(name_)),
    options_{} {
    options_.enforceMatch = compConf.parsedConfig().getBool("enforce-match", true);
    options_.overwriteExisting = compConf.parsedConfig().getBool("overwrite-existing", false);
};

void MetadataMapping::executeImpl(message::Message msg) {
    switch (msg.tag()) {
        case (message::Message::Tag::Field): {
            // TODO optimize for mappings that do not have to match and avoid copying medata

            // Get own copy of metadata in the message
            msg.header().acquireMetadata();

            applyInplace(msg.modifyMetadata());
            executeNext(std::move(msg));
            break;
        }
        default: {
            executeNext(std::move(msg));
            break;
        }
    };
};

void MetadataMapping::applyInplace(message::Metadata& md) const {
    for (const auto& m : mappings_) {
        m.applyInplace(md, options_);
    }
};

void MetadataMapping::print(std::ostream& os) const {
    os << "MetadataMapping(mapping=" << (name_) << ", enforce-match=" << (options_.enforceMatch ? "true" : "false")
       << ", overwrite-existing=" << (options_.overwriteExisting ? "true" : "false") << ")";
}


static ActionBuilder<MetadataMapping> MetadataMappingBuilder("metadata-mapping");
}  // namespace multio::action::metadata_mapping
