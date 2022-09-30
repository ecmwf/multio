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

#ifndef multio_server_actions_Select_H
#define multio_server_actions_Select_H

#include <iosfwd>
#include <vector>
#include <set>
#include <iterator>

#include "multio/action/Action.h"

namespace multio {
namespace action {

using message::Message;

class Select : public Action {
public:
    explicit Select(const ConfigurationContext& confCtx);

    void executeImpl(Message msg) const override;

    void activeFields(std::insert_iterator<std::set<std::string>>& ins) const override;
    void activeCategories(std::insert_iterator<std::set<std::string>>& ins) const override;
    
private:
    void print(std::ostream &os) const override;

    bool matchPlan(const Message& msg) const;

    std::string match_;
    std::vector<std::string> items_;

};

}  // namespace action
}  // namespace multio

#endif
