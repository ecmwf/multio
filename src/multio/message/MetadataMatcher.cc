
#include "multio/message/MetadataMatcher.h"

#include "eckit/config/LocalConfiguration.h"
#include "eckit/value/Value.h" // Remove once config visitor is implemented

#include "multio/message/Message.h"

using eckit::LocalConfiguration;

namespace multio {
namespace message {

//--------------------------------------------------------------------------------------------------

MetadataMatcher::MetadataMatcher(const LocalConfiguration& cfg) {
    for (const auto& k : cfg.keys()) {
        // TODO Use config visitor once added to eckit
        eckit::LocalConfiguration cfgK;
        cfg.get(k, cfgK);
        if (cfgK.get().isList()) {
            auto v = cfg.getStringVector(k);
            matcher_.emplace(k, std::set<std::string>(v.begin(), v.end()));
        }
        else {
            matcher_.emplace(k, std::set<std::string>{cfg.getString(k)});
        }
    }
}

bool MetadataMatcher::matches(const Metadata& md) const {
    for (const auto& kv : matcher_) {
        if (!md.has(kv.first))
            return false;
        if (kv.second.find(md.getString(kv.first)) == kv.second.end())
            return false;
    }
    return true;
}

void MetadataMatcher::print(std::ostream& os) const {
    os << matcher_;
}

//--------------------------------------------------------------------------------------------------

MetadataMatchers::MetadataMatchers(const std::vector<LocalConfiguration>& cfg) {
    for (const auto& m : cfg) {
        matchers_.emplace_back(m);
    }
}

bool MetadataMatchers::matches(const Metadata& md) const {
    for (const auto& matcher : matchers_) {
        if (matcher.matches(md))
            return true;
    }
    return false;
}

bool MetadataMatchers::matches(const Message& msg) const {
    return matches(msg.metadata());
}

void MetadataMatchers::extend(const MetadataMatchers& other) {
    matchers_.insert(matchers_.end(), other.matchers_.begin(), other.matchers_.end());
}

void MetadataMatchers::print(std::ostream& os) const {
    os << matchers_;
}

//--------------------------------------------------------------------------------------------------

}  // namespace message
}  // namespace multio
