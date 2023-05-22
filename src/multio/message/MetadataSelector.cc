
#include "multio/message/MetadataSelector.h"

#include "eckit/config/LocalConfiguration.h"
#include "eckit/value/Value.h"  // Remove once config visitor is implemented

#include "multio/message/Message.h"

using eckit::LocalConfiguration;

namespace multio {
namespace message {

//--------------------------------------------------------------------------------------------------

namespace {
inline bool selectable(const Message& msg) {
    switch (msg.tag()) {
        case Message::Tag::Grib:
        case Message::Tag::Domain:
        case Message::Tag::Mask:
        case Message::Tag::Field:
            return true;
        default:
            return false;
    }
}
}  // namespace


MetadataSelector::MetadataSelector(const LocalConfiguration& cfg) :
    match_(cfg.has("match")
               ? eckit::Optional<MetadataMatchers>{MetadataMatchers{cfg.getSubConfigurations("match")}}
               : eckit::Optional<MetadataMatchers>{}),
    ignore_(cfg.has("ignore")
                ? eckit::Optional<MetadataMatchers>{MetadataMatchers{cfg.getSubConfigurations("ignore")}}
                : eckit::Optional<MetadataMatchers>{}) {}


bool MetadataSelector::matches(const Metadata& md) const {
    return (match_ ? match_->matches(md) : true) && (ignore_ ? !ignore_->matches(md) : true);
}

bool MetadataSelector::matches(const Message& msg) const {
    // Only check matches if the message is selectable, pass through otherwise
    if (!selectable(msg)) {
        return true;
    }
    return matches(msg.metadata());
}

void MetadataSelector::print(std::ostream& os) const {
    os << "{";
    if (match_) {
        os << "match: " << *match_;
        if (ignore_)
            os << ", ";
    }
    if (ignore_) {
        os << "ignore: " << *ignore_;
    }
    os << "}";
}

//--------------------------------------------------------------------------------------------------

MetadataSelectors::MetadataSelectors(const LocalConfiguration& cfg) {
    if (cfg.has("selectors")) {
        for (const auto& m : cfg.getSubConfigurations("selectors")) {
            selectors_.emplace_back(m);
        }
    }
    if (cfg.has("match") || cfg.has("ignore")) {
        selectors_.emplace_back(cfg);
    }
}

MetadataSelectors::MetadataSelectors(const std::vector<LocalConfiguration>& cfg) {
    for (const auto& m : cfg) {
        selectors_.emplace_back(m);
    }
}

bool MetadataSelectors::matches(const Metadata& md) const {
    for (const auto& selector : selectors_) {
        if (selector.matches(md))
            return true;
    }
    return false;
}

bool MetadataSelectors::matches(const Message& msg) const {
    // Code duplication because MetedataSelector::matches may be different on a whole message
    for (const auto& selector : selectors_) {
        if (selector.matches(msg))
            return true;
    }
    return false;
}

void MetadataSelectors::extend(const MetadataSelectors& other) {
    selectors_.insert(selectors_.end(), other.selectors_.begin(), other.selectors_.end());
}
void MetadataSelectors::extend(const MetadataSelector& other) {
    selectors_.emplace_back(other);
}

void MetadataSelectors::print(std::ostream& os) const {
    os << selectors_;
}

//--------------------------------------------------------------------------------------------------

}  // namespace message
}  // namespace multio
