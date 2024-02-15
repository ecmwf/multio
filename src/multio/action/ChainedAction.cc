#include "multio/action/ChainedAction.h"

#include "multio/LibMultio.h"

namespace multio::action {

//----------------------------------------------------------------------------------------------------------------------

ChainedAction::ChainedAction(const ComponentConfiguration& compConf) : Action(compConf) {

    ASSERT(compConf.parsedConfig().has("next"));

    const ComponentConfiguration nextConf = compConf.subComponent("next");
    next_ = ActionFactory::instance().build(nextConf.parsedConfig().getString("type"), nextConf);
}

void ChainedAction::executeNext(message::Message msg) const {
    ASSERT(next_);
    LOG_DEBUG_LIB(LibMultio) << "*** [source = " << msg.source() << ", destination = " << msg.destination()
                             << "] -- Executing action -- " << *next_ << std::endl;
    next_->execute(std::move(msg));
}

void ChainedAction::matchedFields(message::match::MatchReduce& selectors) const {
    // TODO refactor these - passsing down is not directly what we want.
    // Usually we want to only inspect the first action of a plan and check it's requirements...
    // However an observing action, e.g. a print action, should explicitly forward these calls.
    // Then it's possible to perform further analysis ... if not too complex and really required
    // All in all having explicit lists of actions instead of nested actions would help inspecting these things...
    Action::matchedFields(selectors);
    next_->matchedFields(selectors);
}

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio::action
