#pragma once

#include "multio/action/ChainedAction.h"
#include "multio/config/ComponentConfiguration.h"

#include "multio/action/scale/Mapping.h"
#include "multio/action/scale/Scaling.h"

namespace multio::action::scale {

class Scale final : public ChainedAction {
public:
    explicit Scale(const ComponentConfiguration& compConf);  // Constructor declaration

    void executeImpl(message::Message msg) override;

private:
    ScaleScaling scaling_;
    ScaleMapping mapping_;
    std::set<std::string> paramsToScale_;

    template <typename Precision>
    void ScaleMessage(message::Message& msg) const;

    void print(std::ostream&) const override;
};

}  // namespace multio::action::scale
