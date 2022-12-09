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

#ifndef multio_server_Action_H
#define multio_server_Action_H

#include <memory>
#include <map>
#include <set>
#include <iterator>
#include <mutex>

#include "eckit/memory/NonCopyable.h"

#include "multio/action/ActionStatistics.h"
#include "multio/message/Message.h"
#include "multio/util/ConfigurationContext.h"
#include "multio/util/FailureHandling.h"


namespace multio {

namespace message {
class MetadataMatchers;
}

namespace action {

using util::ConfigurationContext;
using util::FailureAware;

//--------------------------------------------------------------------------------------------------

class Action : private eckit::NonCopyable, public FailureAware<util::ComponentTag::Action> {
public:
    explicit Action(const ConfigurationContext& confCtx);
    ~Action() override;

    void execute(message::Message msg) const;

    virtual void matchedFields(message::MetadataMatchers& matchers) const;

    util::FailureHandlerResponse handleFailure(util::OnActionError,
                                               const util::FailureContext&,
                                               util::DefaultFailureState&) const override;

protected:
    ConfigurationContext confCtx_;

    std::string type_;

    mutable ActionStatistics statistics_;

private:


    virtual void executeImpl(message::Message msg) const = 0;

    virtual void print(std::ostream &os) const = 0;

    friend std::ostream& operator<<(std::ostream& os, const Action& a);
};

//--------------------------------------------------------------------------------------------------

class ActionBuilderBase;

class ActionFactory : private eckit::NonCopyable {
private:  // methods
    ActionFactory() {}

public:  // methods
    static ActionFactory& instance();

    void enregister(const std::string& name, const ActionBuilderBase* builder);
    void deregister(const std::string& name);

    void list(std::ostream&);

    Action* build(const std::string&, const ConfigurationContext& confCtx);

private:  // members
    std::map<std::string, const ActionBuilderBase*> factories_;

    std::recursive_mutex mutex_;
};

class ActionBuilderBase : private eckit::NonCopyable {
public:  // methods
    virtual Action* make(const ConfigurationContext& confCtx) const = 0;

protected:  // methods
    ActionBuilderBase(const std::string&);

    virtual ~ActionBuilderBase();

    std::string name_;
};

template <class T>
class ActionBuilder final : public ActionBuilderBase {
    Action* make(const ConfigurationContext& confCtx) const override { return new T(confCtx); }

public:
    ActionBuilder(const std::string& name) : ActionBuilderBase(name) {}
};

//--------------------------------------------------------------------------------------------------


}  // namespace server
}  // namespace multio

#endif
