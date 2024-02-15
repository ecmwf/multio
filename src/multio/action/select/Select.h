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
#include <iterator>
#include <set>
#include <vector>

#include "multio/action/ChainedAction.h"
#include "multio/message/MetadataMatcher.h"

namespace multio::action {

//----------------------------------------------------------------------------------------------------------------------

class Select : public ChainedAction {
public:
    explicit Select(const ComponentConfiguration& compConf);

private:  // methods
    void print(std::ostream& os) const override;

    bool matches(const message::Message& msg) const;

    void executeImpl(message::Message msg) override;

    /// @note This describes an algebra, so the function here can be significantly extended to give helpful return
    void matchedFields(message::match::MatchReduce& selectors) const override;

private:  // members
    message::match::MatchReduce selectors_;
};

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio::action
