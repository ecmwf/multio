/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Parametrization.h"
#include "Metadata.h"

#include <algorithm>

#include "eckit/log/Log.h"


namespace multio {
namespace message {

Parametrization& Parametrization::instance() {
    static Parametrization singleton;
    return singleton;
}

BaseMetadata& Parametrization::get() {
    return data_;
}

void Parametrization::clear() {
    data_ = BaseMetadata{};
}


void Parametrization::update(const BaseMetadata& other) {
    std::lock_guard<std::mutex> lock{mutex_};

    for (const auto& kv : other) {
        if (kv.first.value() != PARAMETRIZATION_PAYLOAD_KEY) {
            update(kv.first, kv.second);
        }
    }
};


void Parametrization::update(const std::string& key, const void* data, std::size_t size) {
    std::lock_guard<std::mutex> lock{mutex_};

    update(key, MetadataValue{std::vector<unsigned char>(static_cast<const unsigned char*>(data),
                                                         static_cast<const unsigned char*>(data) + size)});
}


void Parametrization::update(const Message& msg) {
    const auto& md = msg.metadata();
    auto searchKey = md.find(PARAMETRIZATION_PAYLOAD_KEY);

    if (searchKey != md.end()) {
        auto& payload = msg.payload();
        if (msg.payload().size() == 0) {
            std::ostringstream oss;
            oss << "Parametrization error. Key " << PARAMETRIZATION_PAYLOAD_KEY << " given with value \""
                << searchKey->second << "\" is specified but payload is empty.";
            throw MetadataException(oss.str(), Here());
        }
        update(searchKey->second.get<std::string>(), payload.data(), payload.size());
    }

    update(md);
}


void Parametrization::update(const std::string& key, const MetadataValue& val) {
    auto it = data_.find(key);
    if (it == data_.end()) {
        data_.set(key, val);
    }
    else {
        if (it->second != val) {
            std::ostringstream oss;
            oss << "Parametrization error. Key " << key << " already contains a different value: " << it->second
                << " != " << val;
            throw MetadataException(oss.str(), Here());
        }
    }
}


}  // namespace message
}  // namespace multio
