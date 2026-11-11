/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include "Mars2Mars.h"

#include <sstream>
#include <unordered_set>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/exception/Exceptions.h"

#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/MarsMiscGeo.h"
#include "multio/datamod/core/EntryDumper.h"
#include "multio/datamod/core/EntryParser.h"

namespace multio::action::mars2mars {

namespace dm = multio::datamod;

using message::Message;

namespace {

//----------------------------------------------------------------------------------------------------------------------
// Configuration
//----------------------------------------------------------------------------------------------------------------------

/// Options of the metkit API that make sense for an in-pipeline action.
/// `skipSection3` is deliberately left out: it is a mars2grib concern and requires ECCODES_ECKIT_GEO.
const std::unordered_set<std::string>& supportedMars2marsOptions() {
    static const auto supported = std::unordered_set<std::string>{
        "saveErrorStack", "errorStackPath", "printErrorStackToStdErr", "tryFixBadInput_ZeroAccumulation"};
    return supported;
}

std::unique_ptr<metkit::mars2mars::Mars2Mars> makeConverter(const ComponentConfiguration& compConf) {
    const auto& actionConf = compConf.parsedConfig();

    if (!actionConf.has("mars2mars-options")) {
        return std::make_unique<metkit::mars2mars::Mars2Mars>();
    }

    if (!actionConf.isSubConfiguration("mars2mars-options")) {
        throw eckit::UserError("mars2mars option 'mars2mars-options' must be a configuration section", Here());
    }

    const auto rawConf = actionConf.getSubConfiguration("mars2mars-options");
    eckit::LocalConfiguration validatedConf{};

    for (const auto& key : rawConf.keys()) {
        if (supportedMars2marsOptions().find(key) == supportedMars2marsOptions().end()) {
            throw eckit::UserError("Unsupported mars2mars option 'mars2mars-options." + key + "'", Here());
        }

        if (rawConf.isBoolean(key)) {
            validatedConf.set(key, rawConf.getBool(key));
        }
        else if (rawConf.isString(key)) {
            validatedConf.set(key, rawConf.getString(key));
        }
        else {
            throw eckit::UserError("mars2mars option 'mars2mars-options." + key + "' must be boolean or string",
                                   Here());
        }
    }

    return std::make_unique<metkit::mars2mars::Mars2Mars>(validatedConf);
}

std::optional<message::match::MatchReduce> makeSelector(const ComponentConfiguration& compConf) {
    const auto& actionConf = compConf.parsedConfig();

    if (!actionConf.has("selector")) {
        return std::nullopt;
    }

    if (!actionConf.isSubConfiguration("selector")) {
        throw eckit::UserError("mars2mars option 'selector' must be a configuration section", Here());
    }

    return message::match::MatchReduce::construct(actionConf.getSubConfiguration("selector"));
}

//----------------------------------------------------------------------------------------------------------------------
// Writing the converted dictionaries back into the metadata
//----------------------------------------------------------------------------------------------------------------------

/// Keys the metkit rules may emit in the `misc` result that describe a transformation of the
/// field values rather than of its keys. The action does not touch the payload, so these are errors.
bool isValueTransformation(const std::string& key) {
    return key == "scaleValuesBy" || key == "offsetValuesBy";
}

void setFromConfiguration(message::Metadata& md, const eckit::LocalConfiguration& conf, const std::string& key,
                          const std::string& mdKey) {
    if (conf.isBoolean(key)) {
        md.set(mdKey, conf.getBool(key));
    }
    else if (conf.isIntegral(key)) {
        md.set(mdKey, static_cast<std::int64_t>(conf.getLong(key)));
    }
    else if (conf.isFloatingPoint(key)) {
        md.set(mdKey, conf.getDouble(key));
    }
    else if (conf.isString(key)) {
        md.set(mdKey, conf.getString(key));
    }
    else {
        std::ostringstream oss;
        oss << "mars2mars: unexpected value type for key '" << key << "' in converted dictionary " << conf;
        throw eckit::SeriousBug(oss.str(), Here());
    }
}

/// True when both dictionaries hold `key` with the same scalar type and value.
bool sameValue(const eckit::LocalConfiguration& a, const eckit::LocalConfiguration& b, const std::string& key) {
    if (!a.has(key) || !b.has(key)) {
        return false;
    }
    if (a.isBoolean(key) && b.isBoolean(key)) {
        return a.getBool(key) == b.getBool(key);
    }
    if (a.isIntegral(key) && b.isIntegral(key)) {
        return a.getLong(key) == b.getLong(key);
    }
    if (a.isFloatingPoint(key) && b.isFloatingPoint(key)) {
        return a.getDouble(key) == b.getDouble(key);
    }
    if (a.isString(key) && b.isString(key)) {
        return a.getString(key) == b.getString(key);
    }
    return false;
}

std::string describe(const eckit::LocalConfiguration& mars) {
    std::ostringstream oss;
    oss << "param=" << (mars.has("param") ? mars.getString("param") : "?")
        << ", levtype=" << (mars.has("levtype") ? mars.getString("levtype") : "?");
    if (mars.has("levelist")) {
        oss << ", levelist=" << mars.getString("levelist");
    }
    return oss.str();
}

}  // namespace

//----------------------------------------------------------------------------------------------------------------------

Mars2Mars::Mars2Mars(const ComponentConfiguration& compConf) :
    ChainedAction{compConf}, selector_{makeSelector(compConf)}, converter_{makeConverter(compConf)} {}

void Mars2Mars::executeImpl(Message msg) {
    // Pass through everything that is not a field (flush, notification, ...)
    if (msg.tag() != Message::Tag::Field) {
        executeNext(std::move(msg));
        return;
    }

    if (!matches(msg)) {
        executeNext(std::move(msg));
        return;
    }

    // Get an own copy of the metadata before modifying it
    msg.header().acquireMetadata();
    applyInplace(msg.modifyMetadata());

    executeNext(std::move(msg));
}

bool Mars2Mars::matches(const Message& msg) const {
    return !selector_ || selector_->matches(msg.metadata());
}

void Mars2Mars::applyInplace(message::Metadata& md) {
    util::ScopedTiming timing{statistics_.actionTiming_};

    // The record gives us exactly the set of MARS keys metkit knows about, with their canonical types
    const auto marsRec = dm::readRecord<dm::FullMarsRecord>(md);
    const auto marsIn = dm::dumpRecord<eckit::LocalConfiguration>(marsRec);

    const auto result = converter_->convert(marsIn);

    // Side effects on the misc dictionary: value transformations are not supported here,
    // everything else is handed on as a `misc-*` key for the encoder.
    for (const auto& key : result.misc.keys()) {
        if (isValueTransformation(key)) {
            std::ostringstream oss;
            oss << "mars2mars: rule for field {" << describe(marsIn) << "} -> {" << describe(result.mars)
                << "} requires a transformation of the values ('" << key << "'). This action only renames keys; "
                << "narrow its 'selector' to exclude this field or apply the 'scale' action instead.";
            throw eckit::UserError(oss.str(), Here());
        }
        setFromConfiguration(md, result.misc, key, "misc-" + key);
    }

    // Keys the rules removed (e.g. `number` for type=me)
    for (const auto& key : marsIn.keys()) {
        if (!result.mars.has(key)) {
            md.erase(key);
        }
    }

    // Keys the rules added or changed. Unchanged keys are left alone so that the
    // metadata keeps the representation the producer used.
    for (const auto& key : result.mars.keys()) {
        if (!sameValue(marsIn, result.mars, key)) {
            setFromConfiguration(md, result.mars, key, key);
        }
    }
}

void Mars2Mars::print(std::ostream& os) const {
    os << "Mars2Mars(";
    if (selector_) {
        os << "selector=" << *selector_;
    }
    else {
        os << "selector=all";
    }
    os << ")";
}

//----------------------------------------------------------------------------------------------------------------------

static ActionBuilder<Mars2Mars> Mars2MarsBuilder("mars2mars");

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multio::action::mars2mars
