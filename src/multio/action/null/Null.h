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

#include "multio/action/Action.h"

namespace multio::action::null {

class Null : public Action {
public:
    explicit Null(const ComponentConfiguration& compConf);

    void executeImpl(message::Message msg) override;

private:
    void print(std::ostream& os) const override;
};

}  // namespace multio::action::null
