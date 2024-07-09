
#include "multio/message/MetadataMatcher.h"

#include "eckit/config/LocalConfiguration.h"
#include "eckit/value/Value.h"  // Remove once config visitor is implemented

#include <sstream>

using eckit::LocalConfiguration;

namespace multio::message {

//--------------------------------------------------------------------------------------------------

MetadataMatcher::MetadataMatcher(const LocalConfiguration& cfg) {
    std::map<typename MetadataTypes::KeyType, std::unordered_set<MetadataValue>> matcher;

    for (const auto& k : cfg.keys()) {
        if (cfg.isIntegralList(k)) {
            std::unordered_set<MetadataValue> s;
            for (auto&& vi : cfg.getLongVector(k)) {
                s.emplace(vi);
            }
            matcher.emplace(k, std::move(s));
        }
        else if (cfg.isFloatingPointList(k)) {
            std::unordered_set<MetadataValue> s;
            for (auto&& vi : cfg.getFloatVector(k)) {
                s.emplace(vi);
            }
            matcher.emplace(k, std::move(s));
        }
        else if (cfg.isStringList(k)) {
            std::unordered_set<MetadataValue> s;
            for (auto&& vi : cfg.getStringVector(k)) {
                s.emplace(std::move(vi));
            }
            matcher.emplace(k, std::move(s));
        }
        else if (cfg.isList(k)) {
            std::ostringstream oss;
            oss << "Matcher for key \"" << k << "\""
                << " seems to be a list of mixed or non scalar types - can only handle list of int, float or string: "
                << cfg;
            throw MetadataException(oss.str());
        }
        else {
            auto optMetadataValue = tryToMetadataValue(cfg, k);
            if (!optMetadataValue) {
                std::ostringstream oss;
                oss << "Matcher for key \"" << k << "\" can not be represented by an internal metadata value: " << cfg;
                throw MetadataException(oss.str());
            }
            matcher.emplace(k, std::unordered_set<MetadataValue>{std::move(*optMetadataValue)});
        }
    }

    // Now copy to vector that will get iteratied in future
    matcher_.reserve(matcher.size());
    for (auto&& kv : std::move(matcher)) {
        matcher_.push_back(std::move(kv));
    }
}

bool MetadataMatcher::matches(const Metadata& md) const {
    for (const auto& kv : matcher_) {
        auto searchKey = md.find(kv.first);
        if (searchKey == md.end())
            return false;
        if (kv.second.find(searchKey->second) == kv.second.end())
            return false;
    }
    return true;
}

void MetadataMatcher::print(std::ostream& os) const {
    os << "{";
    bool isFirst = true;
    for (const auto& kv : matcher_) {
        if (!isFirst) {
            os << " ,";
        }
        else {
            isFirst = false;
        }
        os << kv.first.value() << " => {";

        bool isFirst2 = true;
        for (const auto& v : kv.second) {
            if (!isFirst2) {
                os << " ,";
            }
            else {
                isFirst2 = false;
            }
            os << v;
        }
        os << "}";
    }
    os << "}";
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

void MetadataMatchers::extend(const MetadataMatchers& other) {
    matchers_.insert(matchers_.end(), other.matchers_.begin(), other.matchers_.end());
}

void MetadataMatchers::print(std::ostream& os) const {
    os << matchers_;
}

//--------------------------------------------------------------------------------------------------

}  // namespace multio::message
