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

#include "multio/util/PrecisionTag.h"

namespace multio::action {

namespace {
std::set<std::string> fetch_offset_fields(const eckit::Configuration& cfg) {
    const auto& vec = cfg.getStringVector("offset-fields", std::vector<std::string>{});
    return std::set<std::string>{begin(vec), end(vec)};
}

template <typename Precision>
bool setContains(const std::set<Precision>& _set, const Precision& key) {
    return _set.find(key) != std::end(_set);
}

}  // namespace

Mask::Mask(const ComponentConfiguration& compConf) :
    ChainedAction(compConf),
    applyBitmap_{compConf.parsedConfig().getBool("apply-bitmap", true)},
    missingValue_{compConf.parsedConfig().getDouble("missing-value", std::numeric_limits<float>::max())},
    offsetFields_{fetch_offset_fields(compConf.parsedConfig())},
    offsetValue_{compConf.parsedConfig().getDouble("offset-value", 273.15)} {}

void Mask::executeImpl(message::Message msg) {
    if (msg.tag() != message::Message::Tag::Field) {
        executeNext(std::move(msg));
        return;
    }

    executeNext(dispatchPrecisionTag(msg.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        return createMasked<Precision>(std::move(msg));
    }));
}


template <typename Precision>
message::Message Mask::createMasked(message::Message msg) const {
    util::ScopedTiming timing{statistics_.actionTiming_};

    msg.acquire();
    // Now metadata and payload can be modified

    if (applyBitmap_) {
        applyMask<Precision>(msg);
    }

    if (setContains(offsetFields_, msg.name())) {
        applyOffset<Precision>(msg);
    }

    message::Metadata& md = msg.modifyMetadata();
    md.set("missingValue", missingValue_);
    md.set("bitmapPresent", true);

    return msg;
}


template <typename Precision>
void Mask::applyMask(message::Message& msg) const {
    auto const& bkey = domain::Mask::key(msg.metadata());
    auto const& bitmask = domain::Mask::instance().get(bkey);

    if (bitmask.size() * sizeof(Precision) != msg.size()) {
        std::ostringstream oss;
        oss << "Mask::applyMask: Mask for key \"" << bkey << "\" has a size of " << bitmask.size()
            << " but the message contains " << (msg.size() / sizeof(Precision)) << " values. " << std::endl;
        throw eckit::SeriousBug(oss.str(), Here());
    }

    auto git = static_cast<Precision*>(msg.payload().modifyData());

    std::size_t offset = 0;
    for (const auto& valLengthPair : bitmask) {
        if (valLengthPair.first) {
            offset += valLengthPair.second;
        }
        else {
            std::size_t nextOffset = offset + valLengthPair.second;
            for (std::size_t i = offset; i < nextOffset; ++i) {
                git[i] = static_cast<Precision>(missingValue_);
            }
            offset = nextOffset;
        }
    }
}

template <typename Precision>
void Mask::applyOffset(message::Message& msg) const {
    auto const& bkey = domain::Mask::key(msg.metadata());
    auto const& bitmask = domain::Mask::instance().get(bkey);

    ASSERT(bitmask.size() == msg.size() / sizeof(Precision));

    auto git = static_cast<Precision*>(msg.payload().modifyData());

    std::size_t offset = 0;
    for (const auto& valLengthPair : bitmask) {
        if (valLengthPair.first) {
            std::size_t nextOffset = offset + valLengthPair.second;
            for (std::size_t i = offset; i < nextOffset; ++i) {
                git[i] = git[i] + static_cast<Precision>(offsetValue_);
            }
            offset = nextOffset;
        }
        else {
            offset += valLengthPair.second;
        }
    }
}

void Mask::print(std::ostream& os) const {
    os << "Mask(missing=" << missingValue_ << ", offset-fields=" << offsetFields_ << ", offset-value=" << offsetValue_
       << ")";
}

static ActionBuilder<Mask> MaskBuilder("mask");

}  // namespace multio::action
