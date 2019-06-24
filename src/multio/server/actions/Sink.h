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

#ifndef multio_server_actions_Sink_H
#define multio_server_actions_Sink_H

#include <iosfwd>

#include "multio/server/Action.h"

namespace eckit {
class Configuration;
}

namespace multio {

class DataSink;

namespace server {
namespace actions {

class Sink : public Action {
public:
    Sink(const eckit::Configuration& config);

private:
    void execute(Message msg) const override;

    void print(std::ostream& os) const override;

    void write(Message msg) const;

    void flush() const;

    mutable std::unique_ptr<DataSink> dataSink_ = nullptr;
    bool setFilePath_ = false; // Special case only for testing -- remove later
};

}  // namespace actions
}  // namespace server
}  // namespace multio

#endif
