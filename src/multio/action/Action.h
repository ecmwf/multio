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

#include "eckit/log/Statistics.h"
#include "eckit/memory/NonCopyable.h"

#include "multio/action/ActionStatistics.h"
#include "multio/message/Message.h"
#include "multio/util/ConfigurationContext.h"
#include "multio/util/FailureHandling.h"


#include "multio/transport/Transport.h"

namespace multio {
namespace action {

using util::ConfigurationContext;
using util::FailureAware;

//--------------------------------------------------------------------------------------------------

class Action : private eckit::NonCopyable, public FailureAware<util::ComponentTag::Action> {
public:
    Action(const ConfigurationContext& confCtx);
    virtual ~Action();

    // TODO I think we should discuss if passing by value is really the right way
    //      In consideration of multithreading & pipelining, of course we need to deal with copies etc...
    //      However this kind of action pipeline then would also need to be rewritten.
    //      At many places it is just fine to accept const&
    void executeNext(message::Message msg) const;

    void execute(message::Message msg) const;

    virtual void executeImpl(message::Message msg) const = 0;

    virtual util::FailureHandlerResponse handleFailure(util::OnActionError, const util::FailureContext&, util::DefaultFailureState&) const override;

    // May be implemented in a action (i.e. select)
    virtual void activeFields(std::insert_iterator<std::set<std::string>>& ins) const;
    virtual void activeCategories(std::insert_iterator<std::set<std::string>>& ins) const;

    // Computes all active fields of this and following actions
    void computeActiveFields(std::insert_iterator<std::set<std::string>>& ins) const;
    void computeActiveCategories(std::insert_iterator<std::set<std::string>>& ins) const;
    
protected:
    ConfigurationContext confCtx_;

    std::string type_;

    std::unique_ptr<Action> next_;

    mutable ActionStatistics statistics_;

private:

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

    void add(const std::string& name, const ActionBuilderBase* builder);

    void remove(const std::string& name);

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
