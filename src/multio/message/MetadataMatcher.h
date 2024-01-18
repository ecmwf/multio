/*
 * (C) Copyright 2022- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Simon Smart
/// @date Nov 2022

#pragma once

#include <map>
#include <string>
#include <unordered_set>
#include <vector>

#include "eckit/types/Types.h"

#include "Metadata.h"

namespace eckit {
class LocalConfiguration;
}

namespace multio::message {


//--------------------------------------------------------------------------------------------------

class MetadataMatcher {

public:  // methods
    explicit MetadataMatcher(const eckit::LocalConfiguration& cfg);

    bool matches(const Metadata& md) const;

private:  // methods
    friend std::ostream& operator<<(std::ostream& os, const MetadataMatcher& m) {
        m.print(os);
        return os;
    }

    void print(std::ostream& os) const;

private:  // members
    // Use vectorbecause we only iterate over key-pair values
    std::vector<std::pair<typename MetadataTypes::KeyType, std::unordered_set<MetadataValue>>> matcher_;
};

//--------------------------------------------------------------------------------------------------

class MetadataMatchers {

public:  // methods
    MetadataMatchers() = default;
    explicit MetadataMatchers(const std::vector<eckit::LocalConfiguration>& cfg);

    bool matches(const Metadata& msg) const;

    void extend(const MetadataMatchers& other);

private:  // methods
    friend std::ostream& operator<<(std::ostream& os, const MetadataMatchers& m) {
        m.print(os);
        return os;
    }

    void print(std::ostream& os) const;

private:  // members
    std::vector<MetadataMatcher> matchers_;
};

//--------------------------------------------------------------------------------------------------

}  // namespace multio::message

namespace eckit {
template <>
struct VectorPrintSelector<multio::message::MetadataMatcher> {
    typedef VectorPrintSimple selector;
};
}  // namespace eckit
