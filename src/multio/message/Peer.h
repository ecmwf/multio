/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany
/// @author Simon Smart
/// @author Tiago Quintino

/// @date Jan 2019

#pragma once

#include <limits>
#include <string>

namespace multio::message {

class Peer {
public:
    Peer(const std::string& group = "null", size_t id = std::numeric_limits<size_t>::max());
    virtual ~Peer() = default;

    operator std::string();

    bool operator==(const Peer& rhs) const;

    bool operator!=(const Peer& rhs) const;

    bool operator<(const Peer& rhs) const;

    const std::string& group() const;
    size_t id() const;

protected:
    std::string group_;
    size_t id_;

private:  // methods
    void print(std::ostream& out) const;

    friend std::ostream& operator<<(std::ostream& s, const Peer& x) {
        x.print(s);
        return s;
    }
};

}  // namespace multio::message
