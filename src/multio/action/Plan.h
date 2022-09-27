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

#ifndef multio_server_Plan_H
#define multio_server_Plan_H

#include <memory>

#include "eckit/log/Statistics.h"
#include "eckit/memory/NonCopyable.h"

#include "multio/message/Message.h"
#include "multio/transport/Transport.h"
#include "multio/util/ConfigurationContext.h"

namespace multio {
namespace action {

using util::ConfigurationContext;

class Action;

class Plan : private eckit::NonCopyable {
public:
    Plan(const ConfigurationContext& confCtx);
    virtual ~Plan();

    virtual void process(message::Message msg);

    void computeActiveFields(std::insert_iterator<std::set<std::string>>& ins) const;
    void computeActiveCategories(std::insert_iterator<std::set<std::string>>& ins) const;

protected:
    std::string name_;

    std::unique_ptr<Action> root_;
    eckit::Timing timing_;
};

}  // namespace action
}  // namespace multio

#endif
