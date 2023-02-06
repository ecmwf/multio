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

#include <iosfwd>

#include "multio/action/ChainedAction.h"

namespace multio {
namespace action {

class Print : public ChainedAction {
public:
    explicit Print(const ConfigurationContext& config);

    void executeImpl(message::Message msg) override;

private:
    void print(std::ostream& os) const override;

    std::string stream_;

    std::ostream* os_;
    std::string prefix_;
};

}  // namespace action
}  // namespace multio
