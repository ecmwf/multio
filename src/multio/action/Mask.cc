/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "Mask.h"

#include <fstream>
#include <limits>

#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"
#include "eckit/config/Configuration.h"

namespace multio {
namespace action {

Mask::Mask(const eckit::Configuration& config) :
    Action(config) :
    missingValue_{config.getDouble("missing-value", std::numberical_limits<double>::max())} {}

void Mask::execute(message::Message msg) const {

    // Sanity check
    ASSERT(msg.metadata().getLong("levelCount") == 1);

    auto const& bkey = Mask::key(msg.metadata());
    auto const& bitmask = Mask::get(bkey);

    ASSERT(bitmask.size() == msg.size() / sizeof(double));

    auto git = static_cast<double*>(msg.payload().data());

    auto cnt = 0;
    for (const auto bval : bitmask) {
        if (not bval) {
            *git = missingValue_;
            ++git;
            ++cnt;
        }
    }

    eckit::Log::info() << "Field " << msg.name() << " with bitmap " << bkey << ": "
                       << bitmask.size() << " -- Number of land points: " << cnt << std::endl;

    executeNext(msg);
}

void Mask::print(std::ostream& os) const {
    os << "Mask(missing=" << missingValue_ << ")";
}

static ActionBuilder<Mask> MaskBuilder("Mask");

}  // namespace action
}  // namespace multio
