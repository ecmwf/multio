
#include "GribTemplate.h"

#include <cstring>

#include "eckit/exception/Exceptions.h"
#include "eckit/io/Buffer.h"

#include "multio/server/Message.h"

namespace multio {
namespace server {

GribTemplate& GribTemplate::instance() {
    static GribTemplate singleton;
    return singleton;
}

void GribTemplate::add(Message msg) {
    std::lock_guard<std::recursive_mutex> lock{mutex_};

    templates_.push_back(msg);
}

void GribTemplate::list(std::ostream& out) const {
    auto sep = "";
    for (auto const& tmpl : templates_) {
        // Print string here
        out << sep << tmpl.mapping();
        sep = ", ";
    }
}

Message const& GribTemplate::get(const size_t sample_id) const {
    // Must exist
    ASSERT(sample_id <= templates_.size());
    return templates_[sample_id];
}

}  // namespace server
}  // namespace multio
