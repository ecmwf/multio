#pragma once

#include <cstdint>
#include <optional>
#include <ostream>
#include <string>
#include <unordered_map>
#include <vector>

#include "multio/action/ChainedAction.h"
#include "multio/message/Message.h"
#include "multio/message/Metadata.h"
#include "multio/util/config/Parser.h"

namespace multio::action::ifs2mars {

namespace cf = multio::util::config;

enum class Ifs2MarsMode : std::size_t {
    Auto,
    Atmosphere,
    Wam,
};

struct Ifs2MarsConfig {
    std::optional<Ifs2MarsMode> mode;

    static constexpr auto fields_ = std::make_tuple(
        cf::optionalEntry("mode", &Ifs2MarsConfig::mode)
    );
};

class Ifs2Mars final : public ChainedAction {
public:
    explicit Ifs2Mars(const ComponentConfiguration& compConf);

private:
    void executeImpl(message::Message msg) override;
    void print(std::ostream& os) const override;

private:
    Ifs2MarsMode mode_;
};

}  // namespace multio::action::ifs2mars
