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

#ifndef multio_sandbox_actions_Aggregation_H
#define multio_sandbox_actions_Aggregation_H

#include <iosfwd>
#include <unordered_map>
#include <vector>

#include "sandbox/Action.h"

namespace eckit {
class Configuration;
}

namespace multio {
namespace sandbox {
namespace actions {

class Aggregation : public Action {
public:
    Aggregation(const eckit::Configuration& config);

private:
    bool execute(Message msg) override;

    void print(std::ostream& os) const override;

    std::string map_name_;
    std::string field_id_; // It could be the hash of the mars metadata in the future

    std::map<std::string, std::vector<Message>> messages_;

};

}  // namespace actions
}  // namespace sandbox
}  // namespace multio

#endif
