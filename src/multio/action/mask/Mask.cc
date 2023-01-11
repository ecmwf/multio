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

#include "eckit/config/Configuration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/log/Log.h"

#include "multio/domain/Mask.h"
#include "multio/util/ScopedTimer.h"

#include "multio/util/PrecisionTag.h"

namespace multio {
namespace action {

namespace {
std::set<std::string> fetch_offset_fields(const eckit::Configuration& cfg) {
    const auto& vec = cfg.getStringVector("offset-fields", std::vector<std::string>{});
    return std::set<std::string>{begin(vec), end(vec)};
}

template <typename T>
bool setContains(const std::set<T>& _set, const T& key) {
    return _set.find(key) != std::end(_set);
}

}  // namespace

Mask::Mask(const ConfigurationContext& confCtx) :
    ChainedAction(confCtx),
    applyBitmap_{confCtx.config().getBool("apply-bitmap", true)},
    missingValue_{confCtx.config().getDouble("missing-value", std::numeric_limits<double>::max())},
    offsetFields_{fetch_offset_fields(confCtx.config())},
    offsetValue_{confCtx.config().getDouble("offset-value", 273.15)} {}

void Mask::executeImpl(message::Message msg) const {
    executeNext(dispatchPrecisionTag(msg.precision(), [&](auto pt) {
        using PT = typename decltype(pt)::type;
        return createMasked<PT>(std::move(msg));
    }));
}


template <typename T>
message::Message Mask::createMasked(message::Message msg) const {
    util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};

    if (applyBitmap_) {
        applyMask<T>(msg);
    }

    if (setContains(offsetFields_, msg.name())) {
        applyOffset<T>(msg);
    }

    message::Metadata md{msg.metadata()};
    md.set("missingValue", missingValue_);
    md.set("bitmapPresent", true);

    message::Message maskedMsg{message::Message::Header{msg.tag(), msg.source(), msg.destination(), std::move(md)},
                               std::move(msg.payload())};

    return maskedMsg;
}


template <typename T>
void Mask::applyMask(message::Message msg) const {
    auto const& bkey = domain::Mask::key(msg.metadata());
    auto const& bitmask = domain::Mask::instance().get(bkey);

    ASSERT(bitmask.size() == msg.size() / sizeof(T));

    auto git = static_cast<T*>(msg.payload().data());

    for (const auto bval : bitmask) {
        if (not bval) {
            *git = missingValue_;
        }
        ++git;
    }
}

template <typename T>
void Mask::applyOffset(message::Message msg) const {
    auto const& bkey = domain::Mask::key(msg.metadata());
    auto const& bitmask = domain::Mask::instance().get(bkey);

    ASSERT(bitmask.size() == msg.size() / sizeof(T));

    auto git = static_cast<T*>(msg.payload().data());

    for (const auto bval : bitmask) {
        if (bval) {
            *git += offsetValue_;
        }
        ++git;
    }
}

void Mask::print(std::ostream& os) const {
    os << "Mask(missing=" << missingValue_ << ", offset-fields=" << offsetFields_ << ", offset-value=" << offsetValue_
       << ")";
}

static ActionBuilder<Mask> MaskBuilder("mask");

}  // namespace action
}  // namespace multio
