
#include "Metadata.h"

#include <sstream>

#include "eckit/log/JSON.h"
#include "eckit/parser/YAMLParser.h"
#include "eckit/value/Value.h"

namespace multio::message {

// Use overloaded visit to unpack unique_ptr and enforce a copy of the contained metadata object
MetadataValue::MetadataValue(const MetadataValue& other) :
    MetadataValueVariant{other.visit([](auto&& v) { return MetadataValueVariant{std::forward<decltype(v)>(v)}; })} {}

// Use overloaded visit to unpack unique_ptr and enforce a copy of the contained metadata object
MetadataValue& MetadataValue::operator=(const MetadataValue& other) {
    MetadataValueVariant::operator=(
        other.visit([](auto&& v) { return MetadataValueVariant{std::forward<decltype(v)>(v)}; }));
    return *this;
}


Metadata::Metadata(const MapType& values) : values_{values} {};
Metadata::Metadata(MapType&& values) : values_{std::move(values)} {};

Metadata::Metadata() {
    values_.reserve(256);
}
Metadata::Metadata(std::initializer_list<std::pair<const std::string, MetadataValue>> li) : values_{std::move(li)} {}

Metadata::Metadata(const eckit::Value& v) : Metadata(toMetadata(v)) {}
Metadata::Metadata(const eckit::Configuration& c) : Metadata(c.get()) {}


// User-defined conversion to unique_ptr - simply usage
Metadata::operator std::unique_ptr<Metadata>() const& {
    return std::make_unique<Metadata>(*this);
}

Metadata::operator std::unique_ptr<Metadata>() & {
    return std::make_unique<Metadata>(*this);
}

Metadata::operator std::unique_ptr<Metadata>() && {
    return std::make_unique<Metadata>(std::move(*this));
}


MetadataValue&& Metadata::get(const std::string& k) && {
    if (auto searchKey = values_.find(k); searchKey != values_.end()) {
        return std::move(searchKey->second);
    }
    throw MetadataMissingKeyException(k, Here());
}

MetadataValue& Metadata::get(const std::string& k) & {
    if (auto searchKey = values_.find(k); searchKey != values_.end()) {
        return searchKey->second;
    }
    throw MetadataMissingKeyException(k, Here());
}

const MetadataValue& Metadata::get(const std::string& k) const& {
    if (auto searchKey = values_.find(k); searchKey != values_.end()) {
        return searchKey->second;
    }
    throw MetadataMissingKeyException(k, Here());
}


std::optional<MetadataValue> Metadata::getOpt(const std::string& k) && noexcept {
    if (auto search = values_.find(k); search != values_.end()) {
        return std::optional<MetadataValue>{std::move(search->second)};
    }
    return std::optional<MetadataValue>{};
}

std::optional<MetadataValue> Metadata::getOpt(const std::string& k) & noexcept {
    if (auto search = values_.find(k); search != values_.end()) {
        return std::move(search->second);
    }
    return std::optional<MetadataValue>{};
}

std::optional<MetadataValue> Metadata::getOpt(const std::string& k) const& noexcept {
    if (auto search = values_.find(k); search != values_.end()) {
        return std::move(search->second);
    }
    return std::optional<MetadataValue>{};
}

// Specialized get for Metadata
template <>
const Metadata& MetadataValue::get<Metadata>() const& {
    return *get<std::unique_ptr<Metadata>>().get();
}

template <>
Metadata& MetadataValue::get<Metadata>() & {
    return *get<std::unique_ptr<Metadata>>().get();
}

template <>
Metadata&& MetadataValue::get<Metadata>() && {
    return std::move(*(get<std::unique_ptr<Metadata>>()).get());
}


bool Metadata::empty() const noexcept {
    return values_.empty();
}

std::size_t Metadata::size() const noexcept {
    return values_.size();
}

void Metadata::clear() noexcept {
    values_.clear();
}

void Metadata::merge(Metadata& other) {
    values_.merge(other.values_);
}
void Metadata::merge(Metadata&& other) {
    values_.merge(std::move(other.values_));
}


Metadata Metadata::update(const Metadata& other) {
    auto tmp = std::move(values_);
    values_ = other.values_;
    values_.merge(tmp);
    return tmp;
}
Metadata Metadata::update(Metadata&& other) {
    auto tmp = std::move(values_);
    values_ = std::move(other.values_);
    values_.merge(tmp);
    return tmp;
}


MetadataValue& Metadata::operator[](const std::string& key) {
    return values_[key];
}

MetadataValue& Metadata::operator[](std::string&& key) {
    return values_[std::move(key)];
}


namespace {
template <typename T>
void toJSON(const T& v, eckit::JSON& json) {
    json << v;
}

void toJSON(const Null&, eckit::JSON& json) {
    json.null();
};

void toJSON(const Metadata& metadata, eckit::JSON& json);

void toJSON(const MetadataValue& mv, eckit::JSON& json) {
    mv.visit([&json](const auto& v) { toJSON(v, json); });
}

template <typename T>
void toJSON(const std::vector<T>& v, eckit::JSON& json) {
    json.startList();
    for (const auto& vi : v) {
        toJSON(vi, json);
    }
    json.endList();
}

void toJSON(const Metadata& metadata, eckit::JSON& json) {
    json.startObject();
    for (const auto& kv : metadata) {
        json << kv.first;
        toJSON(kv.second, json);
    }
    json.endObject();
}

}  // namespace

std::string toString(const Metadata& metadata) {
    std::stringstream ss;
    eckit::JSON json(ss);
    toJSON(metadata, json);
    return ss.str();
}

std::ostream& operator<<(std::ostream& os, const Metadata& metadata) {
    eckit::JSON json(os);
    toJSON(metadata, json);
    return os;
}

std::ostream& operator<<(std::ostream& os, const MetadataValue& metadataValue) {
    eckit::JSON json(os);
    toJSON(metadataValue, json);
    return os;
}


namespace {
enum class ValueType : unsigned int
{
    NotUsed = 0,
    Double,
    Int,
    Bool,
    String,
    Map,
    List
};

template <ValueType V>
struct ValueTag {};

// JSON/YAMLParser now refactored - that helps for (de)serializing directly without eckit::Value.
// However configurations still use eckit::Value. Therefore these helpers are still required.
template <typename F>
decltype(auto) visitValueType(const eckit::Value& v, F&& f) noexcept {
    if (v.isList()) {
        return std::forward<F>(f)(ValueTag<ValueType::List>{});
    }
    if (v.isMap()) {
        return std::forward<F>(f)(ValueTag<ValueType::Map>{});
    }
    if (v.isNumber()) {
        return std::forward<F>(f)(ValueTag<ValueType::Int>{});
    }
    if (v.isDouble()) {
        return std::forward<F>(f)(ValueTag<ValueType::Double>{});
    }
    if (v.isBool()) {
        return std::forward<F>(f)(ValueTag<ValueType::Bool>{});
    }
    if (v.isString()) {
        return std::forward<F>(f)(ValueTag<ValueType::String>{});
    }
    return std::forward<F>(f)(ValueTag<ValueType::NotUsed>{});
};
}  // namespace

std::optional<MetadataValue> toMetadataValue(const eckit::Value& v) {
    return visitValueType(
        v,
        eckit::Overloaded{
            [&v](ValueTag<ValueType::List>) -> std::optional<MetadataValue> {
                if (!v.size()) {
                    return std::nullopt;
                }
                auto fillVec = [&v](auto vec) {
                    auto size = v.size();
                    vec.reserve(size);
                    for (unsigned int i = 0; i < size; ++i) {
                        vec.push_back(v[i]);
                    }
                    return MetadataValue{std::move(vec)};
                };
                return visitValueType(v[0],
                                      eckit::Overloaded{
                                          [&fillVec](ValueTag<ValueType::Int>) -> std::optional<MetadataValue> {
                                              return fillVec(std::vector<std::int64_t>{});
                                          },
                                          [&fillVec](ValueTag<ValueType::Double>) -> std::optional<MetadataValue> {
                                              return fillVec(std::vector<double>{});
                                          },
                                          [&fillVec](ValueTag<ValueType::Bool>) -> std::optional<MetadataValue> {
                                              return fillVec(std::vector<bool>{});
                                          },
                                          [&fillVec](ValueTag<ValueType::String>) -> std::optional<MetadataValue> {
                                              return fillVec(std::vector<std::string>{});
                                          },
                                          [](auto) -> std::optional<MetadataValue> { return std::nullopt; },
                                      });
            },
            [&v](ValueTag<ValueType::Map>) -> std::optional<MetadataValue> { return toMetadata(v); },
            [&v](ValueTag<ValueType::Int>) -> std::optional<MetadataValue> { return MetadataValue{(std::int64_t)v}; },
            [&v](ValueTag<ValueType::Double>) -> std::optional<MetadataValue> { return MetadataValue{(double)v}; },
            [&v](ValueTag<ValueType::Bool>) -> std::optional<MetadataValue> { return MetadataValue{(bool)v}; },
            [&v](ValueTag<ValueType::String>) -> std::optional<MetadataValue> { return MetadataValue{(std::string)v}; },
            [](auto) -> std::optional<MetadataValue> { return std::nullopt; }});
}

namespace {
// TODO refactor by refactoring eckit::YAMLParser
std::optional<Metadata> toMetadataMaybe(const eckit::Value& v) {
    if (!v.isMap()) {
        return std::nullopt;
    }
    Metadata m;

    eckit::Value keys = v.keys();
    for (unsigned int i = 0; i < keys.size(); ++i) {
        std::string key = keys[i];
        auto mv = toMetadataValue(v[key]);
        if (mv) {
            m.set(key, *mv);
        }
    }

    return m;
}
}  // namespace

Metadata toMetadata(const eckit::Value& value) {
    auto optMetadata = toMetadataMaybe(value);
    if (!optMetadata) {
        std::ostringstream oss;
        oss << "eckit::Value is not a map: " << value;
        throw MetadataException(oss.str(), Here());
    }
    return std::move(*optMetadata);
}

Metadata toMetadata(const std::string& fieldId) {
    std::istringstream in(fieldId);
    eckit::YAMLParser parser(in);
    auto optMetadata = toMetadataMaybe(parser.parse());
    if (!optMetadata) {
        throw MetadataException(std::string("JSON string must start with a map: ") + fieldId, Here());
    }
    return std::move(*optMetadata);
}

//-----------------------------------------------------------------------------

}  // namespace multio::message
