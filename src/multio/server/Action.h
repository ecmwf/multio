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
#include <mutex>

#include "eckit/memory/NonCopyable.h"

#include "multio/server/Message.h"


namespace eckit {
class Configuration;
}

namespace multio {
namespace server {

//----------------------------------------------------------------------------------------------------------------------

class Action : private eckit::NonCopyable {
public:
    Action(const eckit::Configuration& config);
    virtual ~Action() = default;

    Action(Action&& rhs) = default;
    Action& operator=(Action&& rhs) = default;

    virtual void execute(Message msg) const = 0;

protected:

    std::unique_ptr<Action> next_;

private:

    virtual void print(std::ostream &os) const = 0;

    friend std::ostream& operator<<(std::ostream& os, const Action& a) {
        a.print(os);
        return os;
    }
};

//----------------------------------------------------------------------------------------------------------------------

class ActionBuilderBase;

class ActionFactory : private eckit::NonCopyable {
private:  // methods
    ActionFactory() {}

public:  // methods
    static ActionFactory& instance();

    void add(const std::string& name, const ActionBuilderBase* builder);

    void remove(const std::string& name);

    void list(std::ostream&);

    Action* build(const std::string&, const eckit::Configuration& config);

private:  // members
    std::map<std::string, const ActionBuilderBase*> factories_;

    std::recursive_mutex mutex_;
};

class ActionBuilderBase : private eckit::NonCopyable {
public:  // methods
    virtual Action* make(const eckit::Configuration& config) const = 0;

protected:  // methods
    ActionBuilderBase(const std::string&);

    virtual ~ActionBuilderBase();

    std::string name_;
};

template <class T>
class ActionBuilder final : public ActionBuilderBase {
    Action* make(const eckit::Configuration& config) const override { return new T(config); }

public:
    ActionBuilder(const std::string& name) : ActionBuilderBase(name) {}
};

//--------------------------------------------------------------------------------------------------


}  // namespace server
}  // namespace multio

#endif
