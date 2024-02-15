#include "multio/action/Action.h"

#include <fstream>

#include "eckit/config/Configuration.h"
#include "eckit/config/LocalConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"
#include "eckit/runtime/Main.h"

#include "multio/LibMultio.h"
#include "multio/util/logfile_name.h"


namespace multio::action {

using eckit::Configuration;
using eckit::LocalConfiguration;
using eckit::Log;

//----------------------------------------------------------------------------------------------------------------------

Action::Action(const ComponentConfiguration& compConf) :
    FailureAware(compConf), compConf_(compConf), type_{compConf.parsedConfig().getString("type")} {}

Action::~Action() {
    std::ofstream logFile{util::logfile_name(), std::ios_base::app};

    statistics_.report(logFile, type_);
}

void Action::execute(message::Message msg) {
    auto lmsg = msg.logMessage();
    withFailureHandling([&, msg = std::move(msg)]() mutable { executeImpl(std::move(msg)); },
                        [&, lmsg = std::move(lmsg)]() {
                            std::ostringstream oss;
                            oss << *this << " with Message: " << lmsg;
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

void Action::matchedFields(message::match::MatchReduce& selectors) const {}

std::ostream& operator<<(std::ostream& os, const Action& a) {
    a.print(os);
    return os;
}

//---------------------------------------------------------------------------------------------------------------

ActionFactory& ActionFactory::instance() {
    static ActionFactory singleton;
    return singleton;
}

void ActionFactory::enregister(const std::string& name, const ActionBuilderBase* builder) {
    std::lock_guard<std::recursive_mutex> lock{mutex_};
    ASSERT(factories_.find(name) == factories_.end());
    factories_[name] = builder;
}

void ActionFactory::deregister(const std::string& name) {
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

std::unique_ptr<Action> ActionFactory::build(const std::string& name, const ComponentConfiguration& compConf) {
    std::lock_guard<std::recursive_mutex> lock{mutex_};

    LOG_DEBUG_LIB(LibMultio) << "Looking for ActionFactory [" << name << "]" << std::endl;

    auto f = factories_.find(name);

    if (f != factories_.end())
        return f->second->make(compConf);

    Log::error() << "No ActionFactory for [" << name << "]" << std::endl;
    Log::error() << "ActionFactories are:" << std::endl;
    for (auto const& factory : factories_) {
        Log::error() << "   " << factory.first << std::endl;
    }
    throw eckit::SeriousBug(std::string("No ActionFactory called ") + name);
}


ActionBuilderBase::ActionBuilderBase(const std::string& name) : name_(name) {
    ActionFactory::instance().enregister(name, this);
}

ActionBuilderBase::~ActionBuilderBase() {
    ActionFactory::instance().deregister(name_);
}

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio::action
