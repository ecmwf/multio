
#include "MetadataUtils.h"
#include "eckit/exception/Exceptions.h"
#include "multio/message/Metadata.h"


#include "multio/datamod/Glossary.h"


namespace multio::action::scale {

namespace dm = multio::datamod;

std::string extractParam(const multio::message::Metadata& md) {
    std::string cparam{};
    if (auto param = md.getOpt<std::string>(dm::legacy::Param); param) {
        cparam = *param;
    }
    else if (auto paramId = md.getOpt<std::int64_t>(dm::legacy::ParamId); paramId) {
        cparam = std::to_string(*paramId);
    }
    else {
        throw eckit::SeriousBug{"param/paramId metadata not present", Here()};
    }

    return cparam;
}
}  // namespace multio::action::scale
