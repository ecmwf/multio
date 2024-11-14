#pragma once

#include "multio/action/ChainedAction.h"
#include "multio/config/ComponentConfiguration.h"  // Ensure this include is present

namespace multio::action {

class Scale final : public ChainedAction {
public:
    explicit Scale(const ComponentConfiguration& compConf);  // Constructor declaration
    void executeImpl(message::Message) override;

private:

    const double scalingFactor_;  
    
    template <typename Precision>
    message::Message ScaleMessage(message::Message&& msg) const;

    template <typename Precision>
    void scaling(Precision* data, std::size_t size, double scalingFactor) const;
    
    void print(std::ostream&) const override;
};

}  // namespace multio::action::scale
