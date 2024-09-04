/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany
/// @author Simon Smart
/// @author Tiago Quintino

/// @date Jan 2019

#pragma once

#include <iterator>
#include <map>
#include <memory>
#include <mutex>
#include <set>

#include "ActionStatistics.h"
#include "eckit/memory/NonCopyable.h"
#include "multio/config/ComponentConfiguration.h"
#include "multio/message/Message.h"
#include "multio/util/FailureHandling.h"

namespace multio::message::match {
class MatchReduce;
}

namespace multio::action {

using config::ComponentConfiguration;
using util::FailureAware;

struct ActionFailureTraits {
    using OnErrorType = util::OnActionError;
    using FailureOptions = util::DefaultFailureOptions;
    using FailureState = util::DefaultFailureState;
    using TagSequence = util::integer_sequence<OnErrorType, OnErrorType::Propagate, OnErrorType::Recover>;
    static inline std::optional<OnErrorType> parse(const std::string& str) {
        return util::parseErrorTag<OnErrorType, TagSequence>(str);
    }
    static inline OnErrorType defaultOnErrorTag() { return OnErrorType::Propagate; };
    static inline std::string configKey() { return std::string("on-error"); };
    static inline FailureOptions parseFailureOptions(const eckit::Configuration& conf) {
        return util::parseDefaultFailureOptions(conf);
    };
    static inline std::string componentName() { return std::string("Action"); };
};


//--------------------------------------------------------------------------------------------------

class Action : private eckit::NonCopyable, public FailureAware<ActionFailureTraits> {
public:
    explicit Action(const ComponentConfiguration& compConf);

    void execute(message::Message msg);

    virtual void matchedFields(message::match::MatchReduce& selectors) const;

    util::FailureHandlerResponse handleFailure(util::OnActionError, const util::FailureContext&,
                                               util::DefaultFailureState&) const override;

protected:
    ComponentConfiguration compConf_;

    std::string type_;

    mutable ActionStatistics statistics_;

private:
    virtual void executeImpl(message::Message msg) = 0;

    virtual void print(std::ostream& os) const = 0;

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

    std::unique_ptr<Action> build(const std::string&, const ComponentConfiguration& compConf);

private:  // members
    std::map<std::string, const ActionBuilderBase*> factories_;

    std::recursive_mutex mutex_;
};

class ActionBuilderBase : private eckit::NonCopyable {
public:  // methods
    virtual std::unique_ptr<Action> make(const ComponentConfiguration& compConf) const = 0;

protected:  // methods
    ActionBuilderBase(const std::string&);

    virtual ~ActionBuilderBase();

    std::string name_;
};

template <class T>
class ActionBuilder final : public ActionBuilderBase {
    std::unique_ptr<Action> make(const ComponentConfiguration& compConf) const override {
        return std::make_unique<T>(compConf);
    }

public:
    ActionBuilder(const std::string& name) : ActionBuilderBase(name) {}
};

//--------------------------------------------------------------------------------------------------

}  // namespace multio::action
