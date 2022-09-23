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

#ifndef multio_server_actions_Mask_H
#define multio_server_actions_Mask_H

#include <unordered_map>

#include "multio/action/Action.h"
#include "multio/message/ParameterMapping.h"

namespace multio {
namespace action {

class ParameterMapping : public Action {
public:
    explicit ParameterMapping(const ConfigurationContext& confCtx);

    void execute(message::Message msg) const override;

protected:
    void applyInplace(message::Metadata& msg) const;
    message::Metadata apply(const message::Metadata& msg) const;
    message::Metadata apply(message::Metadata&& msg) const;
    
private:
    void print(std::ostream& os) const override;

    std::pair<std::string, std::shared_ptr<std::vector<message::ParameterMapping>>> mappings_;
    bool enforceMatch_;
};

}  // namespace action
}  // namespace multio

#endif
