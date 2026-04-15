/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include "MetadataKeys.h"

#include <unordered_map>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/exception/Exceptions.h"

#include "multio/message/Metadata.h"
#include "multio/message/MetadataException.h"


using FlushKind = multio::action::statistics_mtg2::FlushKind;

namespace {

FlushKind parseFlushKindFromString(const std::string& s) {
    static const std::unordered_map<std::string, FlushKind> map{{"default", FlushKind::Default},
                                                                {"first-step", FlushKind::FirstStep},
                                                                {"step-and-restart", FlushKind::StepAndRestart},
                                                                {"last-step", FlushKind::LastStep},
                                                                {"end-of-simulation", FlushKind::EndOfSimulation},
                                                                {"close-connection", FlushKind::CloseConnection}};
    if (auto it = map.find(s); it != map.end()) {
        return it->second;
    }
    throw multio::message::MetadataException("Unknown FlushKind string: " + s, Here());
}

FlushKind parseFlushKindFromInt(std::int64_t val) {
    static const std::unordered_map<std::int64_t, FlushKind> map{
        {0, FlushKind::FirstStep}, {1, FlushKind::Default},         {2, FlushKind::StepAndRestart},
        {3, FlushKind::LastStep},  {4, FlushKind::EndOfSimulation}, {5, FlushKind::CloseConnection}};
    if (auto it = map.find(val); it != map.end()) {
        return it->second;
    }
    throw multio::message::MetadataException("Unknown FlushKind integer: " + std::to_string(val), Here());
}

std::string flushKindToString(FlushKind v) {
    switch (v) {
        case FlushKind::Default:
            return "default";
        case FlushKind::FirstStep:
            return "first-step";
        case FlushKind::StepAndRestart:
            return "step-and-restart";
        case FlushKind::LastStep:
            return "last-step";
        case FlushKind::EndOfSimulation:
            return "end-of-simulation";
        case FlushKind::CloseConnection:
            return "close-connection";
        default:
            throw eckit::SeriousBug(
                "flushKindToString: Unexpected enum value " + std::to_string(static_cast<std::size_t>(v)), Here());
    }
}

}  // namespace


namespace multio::action::statistics_mtg2 {

/// Parse FlushKind from metadata — handles both string and int64 representations.
/// Uses the untyped getOpt() + visit() pattern to avoid the noexcept bug in BaseMetadata::getOpt<T>.
bool parseEntry(FlushKind& value, std::string_view key, const message::Metadata& md) {
    const std::string k{key};
    auto it = md.find(k);
    if (it == md.end()) {
        return false;
    }
    bool parsed = false;
    it->second.visit(eckit::Overloaded{[&](const std::string& s) {
                                           value = parseFlushKindFromString(s);
                                           parsed = true;
                                       },
                                       [&](std::int64_t v) {
                                           value = parseFlushKindFromInt(v);
                                           parsed = true;
                                       },
                                       [&](const auto&) {
                                           throw message::MetadataException(
                                               "FlushKind must be a string or integer, key: " + k, Here());
                                       }});
    return parsed;
}

void dumpEntry(FlushKind value, std::string_view key, message::Metadata& md) {
    md.set(std::string(key), flushKindToString(value));
}

void dumpConfigEntry(FlushKind value, const std::string& key, eckit::LocalConfiguration& conf) {
    conf.set(key, flushKindToString(value));
}

}  // namespace multio::action::statistics_mtg2
