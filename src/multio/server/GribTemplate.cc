
#include "GribTemplate.h"

#include <cstring>

#include "eckit/exception/Exceptions.h"
#include "eckit/io/Buffer.h"

#include "multio/message/Message.h"

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
        out << sep << tmpl.name();
        sep = ", ";
    }
}

Message const& GribTemplate::get(const std::string& fieldType, bool isSpectral) const {
    ASSERT(templates_.size() == GG2 + 1);

    if (fieldType == "m") {
        return isSpectral ? templates_[SH_ML] : templates_[GG_ML];
    }

    if (isSpectral) {
        return templates_[SH];
    }

    // TODO: Use field parameter to check which version of grib to use
    return templates_[GG]; // Grib version 1

    // Grib templates for wave model are not sent over from client
    // if (fieldType == "wv_spec") {
    //     return templates_[WAM_S];
    // }

    // if (fieldType == "wv_int") {
    //     return templates_[WAM_S];
    // }

    // if (gribVersion == 2) {
    //     return templates_[GG2];
    // }

    // ASSERT(gribVersion == 1);
    // return templates_[GG];
}

}  // namespace server
}  // namespace multio
