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
#include <set>
#include <string>
#include <vector>

#include "eckit/types/Types.h"
#include "eckit/utils/Optional.h"

#include "MetadataMatcher.h"

namespace eckit {
class LocalConfiguration;
}

namespace multio {
namespace message {

class Message;
class Metadata;

//--------------------------------------------------------------------------------------------------

class MetadataSelector {

public:  // methods
    MetadataSelector() = default;
    explicit MetadataSelector(const eckit::LocalConfiguration& cfg);

    bool matches(const Message& msg) const;
    bool matches(const Metadata& msg) const;

private:  // methods
    friend std::ostream& operator<<(std::ostream& os, const MetadataSelector& m) {
        m.print(os);
        return os;
    }

    void print(std::ostream& os) const;

private:  // members
    eckit::Optional<MetadataMatchers> match_;
    eckit::Optional<MetadataMatchers> ignore_;
};

//--------------------------------------------------------------------------------------------------

class MetadataSelectors {

public:  // methods
    MetadataSelectors() = default;
    explicit MetadataSelectors(const eckit::LocalConfiguration& cfg);
    explicit MetadataSelectors(const std::vector<eckit::LocalConfiguration>& cfg);

    bool matches(const Message& msg) const;
    bool matches(const Metadata& msg) const;

    void extend(const MetadataSelector& other);
    void extend(const MetadataSelectors& other);

private:  // methods
    friend std::ostream& operator<<(std::ostream& os, const MetadataSelectors& m) {
        m.print(os);
        return os;
    }

    void print(std::ostream& os) const;

private:  // members
    std::vector<MetadataSelector> selectors_;
};

//--------------------------------------------------------------------------------------------------

}  // namespace message
}  // namespace multio

namespace eckit {
template <>
struct VectorPrintSelector<multio::message::MetadataSelector> {
    typedef VectorPrintSimple selector;
};
}  // namespace eckit
