
#include "multio/message/MetadataMatcher.h"

#include "eckit/config/LocalConfiguration.h"
#include "eckit/value/Value.h"  // Remove once config visitor is implemented

#include <algorithm>
#include <cmath>
#include <set>
#include <sstream>
#include <type_traits>

using eckit::LocalConfiguration;

namespace multio::message::match {

//--------------------------------------------------------------------------------------------------

namespace {

bool isInt(const Number& n) {
    return std::holds_alternative<std::int64_t>(n);
}

double asDouble(const Number& n) {
    return std::visit([](auto v) { return static_cast<double>(v); }, n);
}

// Returns <0, 0, >0. Exact for two integers, otherwise compared as doubles.
int compare(const Number& a, const Number& b) {
    if (isInt(a) && isInt(b)) {
        auto x = std::get<std::int64_t>(a);
        auto y = std::get<std::int64_t>(b);
        return (x < y) ? -1 : ((x > y) ? 1 : 0);
    }
    double x = asDouble(a);
    double y = asDouble(b);
    return (x < y) ? -1 : ((x > y) ? 1 : 0);
}

// v is on the grid from + k * by (k >= 0 is checked by the caller through v >= from)
bool onGrid(const Number& v, const Number& from, const Number& by) {
    if (isInt(v) && isInt(from) && isInt(by)) {
        return (std::get<std::int64_t>(v) - std::get<std::int64_t>(from)) % std::get<std::int64_t>(by) == 0;
    }
    double q = (asDouble(v) - asDouble(from)) / asDouble(by);
    return std::abs(q - std::round(q)) <= 1e-9 * std::max(1.0, std::abs(q));
}

std::ostream& printNumber(std::ostream& os, const Number& n) {
    std::visit([&](auto v) { os << v; }, n);
    return os;
}

const char* comparisonName(Comparison op) {
    switch (op) {
        case Comparison::GreaterThan:
            return "greater-than";
        case Comparison::GreaterEqualTo:
            return "greater-equal-to";
        case Comparison::LowerThan:
            return "lower-than";
        case Comparison::LowerEqualTo:
            return "lower-equal-to";
    }
    return "?";
}

// Reads a scalar or a list of numbers (all integers or all floating point)
std::vector<Number> getNumbers(const LocalConfiguration& cfg, const std::string& name, const std::string& key) {
    std::vector<Number> res;
    if (cfg.isIntegral(name)) {
        res.emplace_back(static_cast<std::int64_t>(cfg.getLong(name)));
    }
    else if (cfg.isFloatingPoint(name)) {
        res.emplace_back(cfg.getDouble(name));
    }
    else if (cfg.isIntegralList(name)) {
        for (auto v : cfg.getLongVector(name)) {
            res.emplace_back(static_cast<std::int64_t>(v));
        }
    }
    else if (cfg.isFloatingPointList(name)) {
        for (auto v : cfg.getDoubleVector(name)) {
            res.emplace_back(v);
        }
    }
    else {
        std::ostringstream oss;
        oss << "Matcher for key \"" << key << "\": \"" << name
            << "\" must be a number or a list of numbers (all integers or all floating point): " << cfg;
        throw MetadataException(oss.str(), Here());
    }
    if (res.empty()) {
        std::ostringstream oss;
        oss << "Matcher for key \"" << key << "\": \"" << name << "\" is an empty list: " << cfg;
        throw MetadataException(oss.str(), Here());
    }
    return res;
}

}  // namespace


std::optional<Number> toNumber(const MetadataValue& v) {
    return v.visit([](const auto& x) -> std::optional<Number> {
        using T = std::decay_t<decltype(x)>;
        if constexpr (std::is_same_v<T, std::int64_t>) {
            return Number{x};
        }
        else if constexpr (std::is_same_v<T, double> || std::is_same_v<T, float>) {
            return Number{static_cast<double>(x)};
        }
        else {
            return std::nullopt;
        }
    });
}


bool Range::matches(const Number& v) const {
    if (compare(v, from) < 0) {
        return false;
    }
    if (to && compare(v, *to) > 0) {
        return false;
    }
    return !by || onGrid(v, from, *by);
}

bool Bound::matches(const Number& v) const {
    int c = compare(v, value);
    switch (op) {
        case Comparison::GreaterThan:
            return c > 0;
        case Comparison::GreaterEqualTo:
            return c >= 0;
        case Comparison::LowerThan:
            return c < 0;
        case Comparison::LowerEqualTo:
            return c <= 0;
    }
    return false;
}


NumericCondition::NumericCondition(const LocalConfiguration& cfg, const std::string& key) {
    static const std::set<std::string> rangeKeys{"from", "to", "by"};
    static const std::vector<std::pair<std::string, Comparison>> boundKeys{
        {"greater-than", Comparison::GreaterThan},
        {"greater-equal-to", Comparison::GreaterEqualTo},
        {"lower-than", Comparison::LowerThan},
        {"lower-equal-to", Comparison::LowerEqualTo}};

    for (const auto& k : cfg.keys()) {
        bool known = rangeKeys.count(k) > 0
                  || std::any_of(boundKeys.begin(), boundKeys.end(), [&](const auto& b) { return b.first == k; });
        if (!known) {
            std::ostringstream oss;
            oss << "Matcher for key \"" << key << "\": unknown condition \"" << k
                << "\" (allowed: from, to, by, greater-than, greater-equal-to, lower-than, lower-equal-to): " << cfg;
            throw MetadataException(oss.str(), Here());
        }
    }

    if (cfg.has("from")) {
        auto from = getNumbers(cfg, "from", key);
        auto to = cfg.has("to") ? getNumbers(cfg, "to", key) : std::vector<Number>{};
        auto by = cfg.has("by") ? getNumbers(cfg, "by", key) : std::vector<Number>{};

        // Lists must have the same length, scalars are broadcast
        std::size_t n = std::max({from.size(), to.size(), by.size()});
        for (const auto* v : {&from, &to, &by}) {
            if (v->size() > 1 && v->size() != n) {
                std::ostringstream oss;
                oss << "Matcher for key \"" << key
                    << "\": \"from\", \"to\" and \"by\" lists must have the same length: " << cfg;
                throw MetadataException(oss.str(), Here());
            }
        }

        auto pick = [](const std::vector<Number>& v, std::size_t i) { return v.size() == 1 ? v[0] : v[i]; };
        for (std::size_t i = 0; i < n; ++i) {
            Range r{pick(from, i), std::nullopt, std::nullopt};
            if (!to.empty()) {
                r.to = pick(to, i);
                if (compare(*r.to, r.from) < 0) {
                    std::ostringstream oss;
                    oss << "Matcher for key \"" << key << "\": \"to\" is lower than \"from\" in segment " << i << ": "
                        << cfg;
                    throw MetadataException(oss.str(), Here());
                }
            }
            if (!by.empty()) {
                r.by = pick(by, i);
                if (asDouble(*r.by) <= 0) {
                    std::ostringstream oss;
                    oss << "Matcher for key \"" << key << "\": \"by\" must be positive in segment " << i << ": " << cfg;
                    throw MetadataException(oss.str(), Here());
                }
            }
            ranges_.push_back(std::move(r));
        }
    }
    else if (cfg.has("to") || cfg.has("by")) {
        std::ostringstream oss;
        oss << "Matcher for key \"" << key << "\": \"to\" and \"by\" require \"from\": " << cfg;
        throw MetadataException(oss.str(), Here());
    }

    for (const auto& [name, op] : boundKeys) {
        if (!cfg.has(name)) {
            continue;
        }
        auto v = getNumbers(cfg, name, key);
        if (v.size() != 1) {
            std::ostringstream oss;
            oss << "Matcher for key \"" << key << "\": \"" << name << "\" must be a single number: " << cfg;
            throw MetadataException(oss.str(), Here());
        }
        bounds_.push_back(Bound{op, v[0]});
    }

    if (ranges_.empty() && bounds_.empty()) {
        std::ostringstream oss;
        oss << "Matcher for key \"" << key << "\": empty condition: " << cfg;
        throw MetadataException(oss.str(), Here());
    }
}

bool NumericCondition::matches(const Number& v) const {
    if (!ranges_.empty()
        && std::none_of(ranges_.begin(), ranges_.end(), [&](const Range& r) { return r.matches(v); })) {
        return false;
    }
    return std::all_of(bounds_.begin(), bounds_.end(), [&](const Bound& b) { return b.matches(v); });
}

void NumericCondition::print(std::ostream& os) const {
    os << "(";
    bool first = true;
    for (const auto& r : ranges_) {
        if (!first) {
            os << " | ";
        }
        first = false;
        os << "from ";
        printNumber(os, r.from);
        if (r.to) {
            os << " to ";
            printNumber(os, *r.to);
        }
        if (r.by) {
            os << " by ";
            printNumber(os, *r.by);
        }
    }
    for (const auto& b : bounds_) {
        if (!first) {
            os << " && ";
        }
        first = false;
        os << comparisonName(b.op) << " ";
        printNumber(os, b.value);
    }
    os << ")";
}

//--------------------------------------------------------------------------------------------------

MatchKeys::MatchKeys(const LocalConfiguration& cfg, Predicate p, bool enforceSameKeyTypes) :
    predicate_{p}, enforceSameKeyTypes_{enforceSameKeyTypes} {
    std::map<typename MetadataTypes::KeyType, KeyMatcher> matcher;

    for (const auto& k : cfg.keys()) {
        if (cfg.isSubConfiguration(k)) {
            // Numeric condition, e.g. step: {from: 0, to: 240, by: 6}
            KeyMatcher km;
            km.conditions.emplace_back(cfg.getSubConfiguration(k), k);
            matcher.emplace(k, std::move(km));
        }
        else if (cfg.isSubConfigurationList(k) && !cfg.getSubConfigurations(k).empty()) {
            // List of numeric conditions - any of them has to match
            KeyMatcher km;
            for (const auto& c : cfg.getSubConfigurations(k)) {
                km.conditions.emplace_back(c, k);
            }
            matcher.emplace(k, std::move(km));
        }
        else if (cfg.isIntegralList(k)) {
            std::unordered_set<MetadataValue> s;
            for (auto&& vi : cfg.getLongVector(k)) {
                s.emplace(vi);
            }
            matcher.emplace(k, KeyMatcher{std::move(s), {}});
        }
        else if (cfg.isFloatingPointList(k)) {
            std::unordered_set<MetadataValue> s;
            for (auto&& vi : cfg.getFloatVector(k)) {
                s.emplace(vi);
            }
            matcher.emplace(k, KeyMatcher{std::move(s), {}});
        }
        else if (cfg.isStringList(k)) {
            std::unordered_set<MetadataValue> s;
            for (auto&& vi : cfg.getStringVector(k)) {
                s.emplace(std::move(vi));
            }
            matcher.emplace(k, KeyMatcher{std::move(s), {}});
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
            matcher.emplace(k, KeyMatcher{std::unordered_set<MetadataValue>{std::move(*optMetadataValue)}, {}});
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
        const KeyMatcher& km = kv.second;
        if (km.values.find(searchKey->second) != km.values.end()) {
            continue;
        }
        if (!km.conditions.empty()) {
            if (auto num = toNumber(searchKey->second)) {
                if (std::any_of(km.conditions.begin(), km.conditions.end(),
                                [&](const NumericCondition& c) { return c.matches(*num); })) {
                    continue;
                }
            }
            else if (enforceSameKeyTypes_) {
                std::ostringstream oss;
                oss << "[enforce-same-key-type] Matcher for key \"" << kv.first
                    << "\" has a numeric condition but the metadata value is not a number." << std::endl;
                oss << "metadata: " << md;
                throw MetadataException(oss.str());
            }
        }
        if (enforceSameKeyTypes_ && km.values.size() > 0) {
            // Note: NEMO needs the possibiity to use empty sets!!!
            // if (kv.second.size() == 0) {
            //    std::ostringstream oss;
            //    oss << "[enforce-same-key-type] Matcher for key \"" << kv.first << "\" is an empty set";
            //    throw MetadataException(oss.str());
            //}

            if (km.values.begin()->index() != searchKey->second.index()) {
                std::ostringstream oss;
                oss << "[enforce-same-key-type] Matcher for key \"" << kv.first
                    << "\" contains values of a different type than the metadata." << std::endl;
                oss << "metadata: " << md << std::endl;
                oss << "match keys: {";
                bool first = true;
                for (const auto& v : km.values) {
                    if (!first) {
                        oss << ", ";
                    }
                    first = false;
                    oss << v;
                }
                oss << "}";
                throw MetadataException(oss.str());
            }
        }

        res = false;
        break;
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
        for (const auto& v : kv.second.values) {
            if (!isFirst2) {
                os << " ,";
            }
            else {
                isFirst2 = false;
            }
            os << v;
        }
        for (const auto& c : kv.second.conditions) {
            if (!isFirst2) {
                os << " ,";
            }
            else {
                isFirst2 = false;
            }
            os << c;
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
