
#include "multio/message/MetadataMatcher.h"

#include "eckit/config/LocalConfiguration.h"
#include "eckit/value/Value.h"  // Remove once config visitor is implemented

#include <sstream>

#include <sstream>

using eckit::LocalConfiguration;

namespace multio::message::match {

//--------------------------------------------------------------------------------------------------

MatchKeys::MatchKeys(const LocalConfiguration& cfg, Predicate p, bool enforceSameKeyTypes) :
    predicate_{p}, enforceSameKeyTypes_{enforceSameKeyTypes} {
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


bool MatchKeys::matches(const Metadata& md) const {
    bool res = true;

    for (const auto& kv : matcher_) {
        auto searchKey = md.find(kv.first);
        if (searchKey == md.end()) {
            res = false;
            break;
        }
        if (kv.second.find(searchKey->second) == kv.second.end()) {
            if (enforceSameKeyTypes_ && kv.second.size() > 0 ) {
                // Note: NEMO needs the possibiity to use empty sets!!!
                //if (kv.second.size() == 0) {
                //    std::ostringstream oss;
                //    oss << "[enforce-same-key-type] Matcher for key \"" << kv.first << "\" is an empty set";
                //    throw MetadataException(oss.str());
                //}

                if (kv.second.begin()->index() != searchKey->second.index()) {
                    std::ostringstream oss;
                    oss << "[enforce-same-key-type] Matcher for key \"" << kv.first
                        << "\" contains values of a different type than the metadata." << std::endl;
                    oss << "metadata: " << md << std::endl;
                    oss << "match keys: {";
                    bool first = true;
                    for (const auto& v : kv.second) {
                        if (!first) {
                            oss << ", ";
                            first = false;
                        }
                        oss << v;
                    }
                    oss << "}";
                    throw MetadataException(oss.str());
                }
            }

            res = false;
            break;
        }
    }

    if (predicate_ == Predicate::Negate) {
        return !res;
    }
    else {
        return res;
    }
}

namespace {

Predicate negatePredicate(Predicate p) {
    switch (p) {
        case Predicate::None:
            return Predicate::Negate;
        default:
            return Predicate::None;
    }
}

}  // namespace

void MatchKeys::negate() {
    predicate_ = negatePredicate(predicate_);
}

void MatchKeys::applyPredicate(Predicate p) {
    if (p == Predicate::Negate) {
        this->negate();
    }
}


void MatchKeys::print(std::ostream& os) const {
    if (predicate_ == Predicate::Negate) {
        os << "!(";
    }
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
    if (predicate_ == Predicate::Negate) {
        os << ")";
    }
}

//--------------------------------------------------------------------------------------------------


namespace {

Predicate invert(Predicate p) {
    switch (p) {
        case Predicate::None:
            return Predicate::Negate;
        default:
            return Predicate::None;
    }
}

std::variant<MatchKeys, MatchReduce> constructMatchIgnore(const LocalConfiguration& cfg, const std::string& key,
                                                          Predicate p, bool enforceSameKeyTypes) {
    eckit::LocalConfiguration cfgK;
    cfg.get(key, cfgK);

    if (cfg.isSubConfiguration(key)) {
        return MatchKeys{cfg.getSubConfiguration(key), p, enforceSameKeyTypes};
    }
    else if (cfg.isSubConfigurationList(key)) {
        auto v = cfg.getSubConfigurations(key);

        if (v.size() == 1) {
            return MatchKeys{v[0], p, enforceSameKeyTypes};
        }
        else {
            MatchReduce res(Reduce::Or, p);
            for (auto& vi : v) {
                res.extend(MatchKeys{vi, Predicate::None, enforceSameKeyTypes});
            }
            return res;
        }
    }
    else {
        std::ostringstream oss;
        oss << "MetadataMatcher: The block for \"" << key
            << "\" is expected to be a map or list of maps. This one is something else: " << cfg;
        throw MetadataException(oss.str(), Here());
    }
}

}  // namespace

MatchReduce MatchReduce::construct(const LocalConfiguration& cfg, Predicate p, bool enforceSameKeyTypesParent) {
    // TODO - we have no options to check this anymore
    // if (!cfg.get().isMap()) {
    //     std::ostringstream oss;
    //     oss << "MetadataMatcher: Expected a map: " << cfg;
    //     throw MetadataException(oss.str(), Here());
    // }

    bool hasAny = cfg.has("any");
    bool hasAll = cfg.has("all");
    bool hasMatch = cfg.has("match");
    bool hasIgnore = cfg.has("ignore");
    bool hasMatchOrIgnore = hasMatch || hasIgnore;
    bool hasNot = cfg.has("not");

    bool enforceSameKeyTypes
        = cfg.has("enforce-same-key-types") ? cfg.getBool("enforce-same-key-types") : enforceSameKeyTypesParent;

    int checkKeySum = ((int)hasAny + (int)hasAll + (int)hasNot + (int)hasMatchOrIgnore);

    if (checkKeySum > 1) {
        std::ostringstream oss;
        oss << "MetadataMatcher: can only have either \"any\", \"all\", \"not\" or directly a \"match\"/\"ignore\" "
               "configuration but not their combination: "
            << cfg;
        throw MetadataException(oss.str(), Here());
    }
    if (checkKeySum == 0) {
        std::ostringstream oss;
        oss << "MetadataMatcher: Require  a \"any\", \"all\", \"not\", \"match\" or \"ignore\" configuration block: "
            << cfg;
        throw MetadataException(oss.str(), Here());
    }

    if (hasNot) {
        return construct(cfg.getSubConfiguration("not"), invert(p), enforceSameKeyTypes);
    }
    else if (hasMatchOrIgnore) {
        if (hasMatch && hasIgnore) {
            MatchReduce res{Reduce::And, p};
            std::visit([&](auto&& v) { res.extend(std::move(v)); },
                       constructMatchIgnore(cfg, "match", Predicate::None, enforceSameKeyTypes));
            std::visit([&](auto&& v) { res.extend(std::move(v)); },
                       constructMatchIgnore(cfg, "ignore", Predicate::Negate, enforceSameKeyTypes));
            return res;
        }
        else {
            return std::visit(eckit::Overloaded{[&](MatchKeys&& mk) -> MatchReduce {
                                                    MatchReduce res{Reduce::Or, Predicate::None};
                                                    res.extend(std::move(mk));
                                                    return res;
                                                },
                                                [&](MatchReduce&& mr) -> MatchReduce { return mr; }},
                              constructMatchIgnore(cfg, hasMatch ? "match" : "ignore",
                                                   hasMatch ? p : negatePredicate(p), enforceSameKeyTypes));
        }
    }
    else {
        // all or any
        MatchReduce res{hasAll ? Reduce::And : Reduce::Or, p};
        std::string key = hasAll ? "all" : "any";

        if (!cfg.isSubConfigurationList(key)) {
            std::ostringstream oss;
            oss << "MetadataMatcher: The block for \"" << key << "\" is expected to be a list of maps: " << cfg;
            throw MetadataException(oss.str(), Here());
        }

        auto v = cfg.getSubConfigurations(key);
        for (auto& vi : v) {
            res.extend(construct(vi, Predicate::None, enforceSameKeyTypes));
        }

        return res;
    }
}

MatchReduce::MatchReduce(Reduce r, Predicate p) : reduce_{r}, predicate_{p} {}

MatchReduce::MatchReduce(const eckit::LocalConfiguration& cfg, Predicate p) :
    MatchReduce{MatchReduce::construct(cfg, p)} {}

namespace {

bool matchesVariant(typename MatchReduce::Elem const& matcher, const Metadata& md) {
    return std::visit(eckit::Overloaded{[&](const MatchKeys& mk) { return mk.matches(md); },
                                        [&](const std::shared_ptr<MatchReduce>& mr) { return mr->matches(md); }},
                      matcher);
}

}  // namespace

bool MatchReduce::matches(const Metadata& md) const {
    bool res;
    if (reduce_ == Reduce::Or) {
        // Any: Reduce or
        res = false;
        for (const auto& matcher : matchers_) {
            if (matchesVariant(matcher, md)) {
                res = true;
                break;
            }
        }
    }
    else {
        // All: Reduce and
        res = true;
        for (const auto& matcher : matchers_) {
            if (!matchesVariant(matcher, md)) {
                res = false;
                break;
            }
        }
    }

    if (predicate_ == Predicate::Negate) {
        return !res;
    }
    else {
        return res;
    }
}

bool MatchReduce::isEmpty() const {
    return matchers_.empty();
}

void MatchReduce::extend(const MatchKeys& mk) {
    matchers_.push_back(mk);
}

void MatchReduce::extend(MatchKeys&& mk) {
    matchers_.push_back(std::move(mk));
}

void MatchReduce::extend(const MatchReduce& mr) {
    matchers_.push_back(std::make_shared<MatchReduce>(mr));
}

void MatchReduce::extend(MatchReduce&& mr) {
    matchers_.push_back(std::make_shared<MatchReduce>(std::move(mr)));
}

void MatchReduce::extend(typename MatchReduce::Elem const& e) {
    matchers_.push_back(e);
}

void MatchReduce::extend(typename MatchReduce::Elem&& e) {
    matchers_.push_back(std::move(e));
}


void MatchReduce::print(std::ostream& os) const {
    os << "MatchReduce(" << (reduce_ == Reduce::And ? "&&" : "||") << ", "
       << (predicate_ == Predicate::None ? "+" : "-") << ", [";
    bool first = true;
    for (const auto& elem : matchers_) {
        if (!first) {
            os << ", ";
        }
        first = false;

        std::visit(eckit::Overloaded{[&](const MatchKeys& mk) { os << mk; },
                                     [&](const std::shared_ptr<MatchReduce>& mr) { os << *mr.get(); }},
                   elem);
    }
    os << "])";
}

void MatchReduce::negate() {
    predicate_ = negatePredicate(predicate_);
}

void MatchReduce::applyPredicate(Predicate p) {
    if (p == Predicate::Negate) {
        this->negate();
    }
}

//--------------------------------------------------------------------------------------------------

}  // namespace multio::message::match
