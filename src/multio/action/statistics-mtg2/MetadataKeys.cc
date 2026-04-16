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
#include "multio/datamod/core/DataModellingException.h"

using FlushKind = multio::action::statistics_mtg2::FlushKind;

std::string multio::datamod::DumpType<FlushKind>::dump(FlushKind v) {
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
            throw multio::datamod::DataModellingException(
                "DumpType<FlushKind>::dump: Unexpected enum value " + std::to_string(static_cast<std::size_t>(v)),
                Here());
    }
}

FlushKind multio::datamod::ParseType<FlushKind>::parse(const std::string& s) {
    static const std::unordered_map<std::string, FlushKind> map{
        {"default", FlushKind::Default},
        {"first-step", FlushKind::FirstStep},
        {"step-and-restart", FlushKind::StepAndRestart},
        {"last-step", FlushKind::LastStep},
        {"end-of-simulation", FlushKind::EndOfSimulation},
        {"close-connection", FlushKind::CloseConnection}};
    if (auto it = map.find(s); it != map.end()) {
        return it->second;
    }
    throw multio::datamod::DataModellingException("ParseType<FlushKind>::parse: Unknown FlushKind string: " + s,
                                                  Here());
}

FlushKind multio::datamod::ParseType<FlushKind>::parse(std::int64_t val) {
    static const std::unordered_map<std::int64_t, FlushKind> map{
        {0, FlushKind::FirstStep},     {1, FlushKind::Default},          {2, FlushKind::StepAndRestart},
        {3, FlushKind::LastStep},      {4, FlushKind::EndOfSimulation},  {5, FlushKind::CloseConnection}};
    if (auto it = map.find(val); it != map.end()) {
        return it->second;
    }
    throw multio::datamod::DataModellingException(
        "ParseType<FlushKind>::parse: Unknown FlushKind integer: " + std::to_string(val), Here());
}
