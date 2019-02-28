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

#ifndef multio_sandbox_actions_Sink_H
#define multio_sandbox_actions_Sink_H

#include <iosfwd>

#include "sandbox/Action.h"

namespace eckit {
class Configuration;
}

namespace multio {

class DataSink;

namespace sandbox {
namespace actions {

class Sink : public Action {
public:
    Sink(const eckit::Configuration& config);

private:
    bool execute(Message msg) override;

    void print(std::ostream& os) const override;

    bool write(Message msg) const;

    bool flush() const;

    mutable std::unique_ptr<DataSink> dataSink_ = nullptr;
};

}  // namespace actions
}  // namespace sandbox
}  // namespace multio

#endif
