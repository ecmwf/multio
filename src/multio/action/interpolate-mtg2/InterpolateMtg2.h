/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 *
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


#pragma once

#include "multio/action/ChainedAction.h"
#include "multio/util/config/Parser.h"

#include <optional>

namespace multio::action::interpolate_mtg2 {

enum class ResolutionKeyword
{
    Low,
    Standard,
    High
};

}  // namespace multio::action::interpolate_mtg2

template <>
struct multio::util::config::detail::EnumTrait<multio::action::interpolate_mtg2::ResolutionKeyword> {
    static constexpr std::array values{
        std::pair{multio::action::interpolate_mtg2::ResolutionKeyword::Low, "low"},
        std::pair{multio::action::interpolate_mtg2::ResolutionKeyword::Standard, "standard"},
        std::pair{multio::action::interpolate_mtg2::ResolutionKeyword::High, "high"},
    };
};

namespace multio::action::interpolate_mtg2 {

namespace cf = multio::util::config;

struct AdditionalMetadata {
    std::optional<ResolutionKeyword> resolution;

    static constexpr auto fields_
        = std::make_tuple(multio::util::config::optionalEntry("resolution", &AdditionalMetadata::resolution));
};

struct MIROptions {
    std::optional<std::string> interpolation;        /// Previously just checked for "matrix"
    std::optional<std::string> interpolationMatrix;  /// Path to weights file. If interpolation=matrix, this argument
                                                     /// will contain a generated key that is level dependent.
    std::optional<std::string> intermediateInterpolation;  /// Useful for fesom to HEALPix through an intermediate
                                                           /// interpolation - set it to "nearest-neighbour"
    std::optional<std::string> intgrid;  /// Useful for fesom to HEALPix through an intermediate interpolation - sit it
                                         /// to an Octahedral grid with suitable resolution O80
    std::optional<bool> caching;         /// Forwarded to MIR to enable its internal caching mechanism.

    static constexpr auto fields_ = std::make_tuple(
        multio::util::config::optionalEntry("interpolation", &MIROptions::interpolation),
        multio::util::config::optionalEntry("interpolation-matrix", &MIROptions::interpolationMatrix),
        multio::util::config::optionalEntry("intermediate-interpolation", &MIROptions::intermediateInterpolation),
        multio::util::config::optionalEntry("intgrid", &MIROptions::intgrid),
        multio::util::config::optionalEntry("caching", &MIROptions::caching));
};

struct OutputConfig {
    std::string grid;    /// MARS Grid identifier - the only required key. Additional options are grouped in `options`
    bool enable = true;  /// Separate option to enable or disable the output

    std::string cachePath = ".";  /// Used to build a cache key for the special case if interpolation=matrix is passed
    MIROptions options = {};
    AdditionalMetadata additionalMetadata = {};  /// Additional metadata for the multio output message

    static constexpr auto fields_ = std::make_tuple(
        cf::requiredEntry("grid", &OutputConfig::grid), cf::optionalEntry("enable", &OutputConfig::enable),
        cf::optionalEntry("cache-path", &OutputConfig::cachePath), cf::optionalEntry("options", &OutputConfig::options),
        cf::optionalEntry("additional-metadata", &OutputConfig::additionalMetadata));
};

///  The interpolate-mtg2 option allows multiplexing conversions to a list of specified output grids
struct Options {
    std::vector<OutputConfig> outputs;

    static constexpr auto fields_ = std::make_tuple(cf::requiredEntry("outputs", &Options::outputs));
};


/**
 * \class MultIO Action for interpolation/regridding
 */
class InterpolateMtg2 final : public ChainedAction {
public:
    explicit InterpolateMtg2(const ComponentConfiguration& compConf);

private:
    template <typename T>
    void interpolateMessage(message::Message&&) const;

    void print(std::ostream&) const override;
    void executeImpl(message::Message) override;

    Options opts_;
};


}  // namespace multio::action::interpolate_mtg2
