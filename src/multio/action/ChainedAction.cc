#include "multio/action/ChainedAction.h"

#include "multio/LibMultio.h"

namespace multio {
namespace action {

//----------------------------------------------------------------------------------------------------------------------

ChainedAction::ChainedAction(const ConfigurationContext& confCtx) :
    Action(confCtx) {

    ASSERT(confCtx.config().has("next"));

    const ConfigurationContext nextCtx = confCtx.subContext("next", util::ComponentTag::Action);
    next_.reset(ActionFactory::instance().build(nextCtx.config().getString("type"), nextCtx));
}

void ChainedAction::executeNext(message::Message msg) const {
    ASSERT(next_);
    LOG_DEBUG_LIB(LibMultio) << "*** [source = " << msg.source() << ", destination = " << msg.destination()
                             << "] -- Executing action -- " << *next_ << std::endl;
    next_->execute(std::move(msg));
}

void ChainedAction::activeFields(std::insert_iterator<std::set<std::string>>& ins) const {
    Action::activeFields(ins);
    next_->activeFields(ins);
}

void ChainedAction::activeCategories(std::insert_iterator<std::set<std::string>>& ins) const {
    Action::activeCategories(ins);
    next_->activeCategories(ins);
}

//----------------------------------------------------------------------------------------------------------------------

} // namespace action
} // namespace multio
