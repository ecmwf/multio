#include "multio/action/Action.h"

#include <fstream>

#include "eckit/config/Configuration.h"
#include "eckit/config/LocalConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"
#include "eckit/runtime/Main.h"

#include "multio/LibMultio.h"
#include "multio/util/logfile_name.h"


namespace multio {
namespace action {

using eckit::LocalConfiguration;
using eckit::Configuration;
using eckit::Log;

//----------------------------------------------------------------------------------------------------------------------

Action::Action(const ConfigurationContext& confCtx) :
    FailureAware(confCtx), confCtx_(confCtx), type_{confCtx.config().getString("type")} {
}

Action::~Action() {
    std::ofstream logFile{util::logfile_name(), std::ios_base::app};

    statistics_.report(logFile, type_);
}

void Action::execute(message::Message msg) const {
    withFailureHandling([&]() { executeImpl(std::move(msg)); },
                        [&, msg]() {
                            std::ostringstream oss;
                            oss << *this << " with Message: " << msg;
                            return oss.str();
                        });
}

util::FailureHandlerResponse Action::handleFailure(util::OnActionError t, const util::FailureContext&,
                                                   util::DefaultFailureState&) const {
    if (t == util::OnActionError::Recover) {
        return util::FailureHandlerResponse::Retry;
    }
    return util::FailureHandlerResponse::Rethrow;
};


void Action::activeFields(std::insert_iterator<std::set<std::string>>& ins) const {}

void Action::activeCategories(std::insert_iterator<std::set<std::string>>& ins) const {}

std::ostream& operator<<(std::ostream& os, const Action& a) {
    a.print(os);
    return os;
}

//---------------------------------------------------------------------------------------------------------------

ActionFactory& ActionFactory::instance() {
    static ActionFactory singleton;
    return singleton;
}

void ActionFactory::add(const std::string& name, const ActionBuilderBase* builder) {
    std::lock_guard<std::recursive_mutex> lock{mutex_};
    ASSERT(factories_.find(name) == factories_.end());
    factories_[name] = builder;
}

void ActionFactory::remove(const std::string& name) {
    std::lock_guard<std::recursive_mutex> lock{mutex_};
    ASSERT(factories_.find(name) != factories_.end());
    factories_.erase(name);
}

void ActionFactory::list(std::ostream& out) {
    std::lock_guard<std::recursive_mutex> lock{mutex_};

    const char* sep = "";
    for (auto const& sinkFactory : factories_) {
        out << sep << sinkFactory.first;
        sep = ", ";
    }
}

Action* ActionFactory::build(const std::string& name, const ConfigurationContext& confCtx) {
    std::lock_guard<std::recursive_mutex> lock{mutex_};
    ASSERT(confCtx.componentTag() == util::ComponentTag::Action);

    LOG_DEBUG_LIB(LibMultio) << "Looking for ActionFactory [" << name << "]" << std::endl;

    auto f = factories_.find(name);

    if (f != factories_.end())
        return f->second->make(confCtx);

    Log::error() << "No ActionFactory for [" << name << "]" << std::endl;
    Log::error() << "ActionFactories are:" << std::endl;
    for (auto const& factory : factories_) {
        Log::error() << "   " << factory.first << std::endl;
    }
    throw eckit::SeriousBug(std::string("No ActionFactory called ") + name);
}


ActionBuilderBase::ActionBuilderBase(const std::string& name) : name_(name) {
    ActionFactory::instance().add(name, this);
}

ActionBuilderBase::~ActionBuilderBase() {
    ActionFactory::instance().remove(name_);
}

//----------------------------------------------------------------------------------------------------------------------

}  // namespace action
}  // namespace multio
