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
#include <unordered_map>

#include "multio/action/ChainedAction.h"
#include "multio/action/aggregate/AggregationCatalogue.h"

namespace multio {
namespace action {

using message::Message;

class Aggregate : public ChainedAction {
public:
    explicit Aggregate(const ConfigurationContext& confCtx);

    void executeImpl(Message msg) const override;

private:
    void print(std::ostream& os) const override;

    bool handleField(const Message& msg) const;
    bool handleFlush(const Message& msg) const;

    Message createGlobalField(const std::string& msg) const;
    bool allPartsArrived(const Message& msg) const;

    auto flushCount(const Message& msg) const;

    mutable AggregationCatalogue aggCatalogue_;
    mutable std::map<std::string, unsigned int> flushes_;
};

}  // namespace action
}  // namespace multio
