
#include "MetadataUtils.h"
#include "eckit/exception/Exceptions.h"
#include "multio/message/Metadata.h"


#include "multio/message/Glossary.h"


namespace multio::action {
using message::glossary;

std::string extractParam(const multio::message::Metadata& md) {
    std::string cparam{};
    if (auto param = md.getOpt<std::string>(glossary().param); param) {
        cparam = *param;
    } else if (auto paramId = md.getOpt<std::int64_t>(glossary().paramId); paramId) {
        cparam = std::to_string(*paramId);
    }
    else {
        throw eckit::SeriousBug{"param/paramId metadata not present", Here()};
    }

    return cparam;
}
}