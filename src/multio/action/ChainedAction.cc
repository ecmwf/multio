#include "multio/action/ChainedAction.h"

#include "multio/LibMultio.h"

namespace multio::action {

//----------------------------------------------------------------------------------------------------------------------

ChainedAction::ChainedAction(const ComponentConfiguration& compConf) : Action(compConf) {

    ASSERT(compConf.YAML().has("next"));

    const ComponentConfiguration nextConf = compConf.subComponent("next", config::ComponentTag::Action);
    next_ = ActionFactory::instance().build(nextConf.YAML().getString("type"), nextConf);
}

void ChainedAction::executeNext(message::Message msg) const {
    ASSERT(next_);
    LOG_DEBUG_LIB(LibMultio) << "*** [source = " << msg.source() << ", destination = " << msg.destination()
                             << "] -- Executing action -- " << *next_ << std::endl;
    next_->execute(std::move(msg));
}

void ChainedAction::matchedFields(message::MetadataSelectors& selectors) const {
    Action::matchedFields(selectors);
    next_->matchedFields(selectors);
}

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio::action
