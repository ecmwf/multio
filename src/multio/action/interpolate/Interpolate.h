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

namespace multio::action::interpolate {

enum class ResolutionKeyword
{
    Low,
    Standard,
    High
};

}

template <>
struct multio::util::config::detail::EnumTrait<multio::action::interpolate::ResolutionKeyword> {
    static constexpr std::array values{
        std::pair{multio::action::interpolate::ResolutionKeyword::Low, "low"},
        std::pair{multio::action::interpolate::ResolutionKeyword::Standard, "standard"},
        std::pair{multio::action::interpolate::ResolutionKeyword::High, "high"},
    };
};

namespace multio::action::interpolate {

namespace cf = multio::util::config;

struct AdditionalMetadata {
    std::optional<ResolutionKeyword> resolution;

    static constexpr auto fields_
        = std::make_tuple(multio::util::config::optionalEntry("resolution", &AdditionalMetadata::resolution));
};

struct Options {
    std::string input;

    // post proc keys
    std::optional<std::string> grid;
    std::optional<std::array<double, 4>> area;

    // Pass through options
    std::optional<eckit::LocalConfiguration> options;

    std::optional<std::string> cachePath;
    std::optional<AdditionalMetadata> additionalMetadata = {};

    // Additional postproc keys - copied for completeness and validation --- usually passed through as options, but
    // previously it was working directly as well
    std::optional<std::string> accuracy;
    std::optional<std::string> bitmap;
    std::optional<std::string> format;
    std::optional<std::int64_t> frame;
    std::optional<std::string> gaussian;
    std::optional<std::string> interpolation;
    std::optional<std::string> packing;
    std::optional<std::string> resol;
    std::optional<double> rotation;
    std::optional<std::string> intgrid;
    std::optional<std::string> truncation;
    std::optional<std::string> process;

    static constexpr auto fields_ = std::make_tuple(
        cf::optionalEntry("input", &Options::input), cf::optionalEntry("grid", &Options::grid),
        cf::optionalEntry("area", &Options::area), cf::optionalEntry("options", &Options::options),
        cf::optionalEntry("cache-path", &Options::cachePath),
        cf::optionalEntry("additional-metadata", &Options::additionalMetadata),
        cf::optionalEntry("accuracy", &Options::accuracy), cf::optionalEntry("bitmap", &Options::bitmap),
        cf::optionalEntry("format", &Options::format), cf::optionalEntry("frame", &Options::frame),
        cf::optionalEntry("gaussian", &Options::gaussian), cf::optionalEntry("interpolation", &Options::interpolation),
        cf::optionalEntry("packing", &Options::packing), cf::optionalEntry("resol", &Options::resol),
        cf::optionalEntry("rotation", &Options::rotation), cf::optionalEntry("intgrid", &Options::intgrid),
        cf::optionalEntry("truncation", &Options::truncation), cf::optionalEntry("process", &Options::process));
};


/**
 * \class MultIO Action for interpolation/regridding
 */
class Interpolate final : public ChainedAction {
public:
    explicit Interpolate(const ComponentConfiguration& compConf);

private:
    template <typename T>
    message::Message interpolateMessage(message::Message&&) const;

    void print(std::ostream&) const override;
    void executeImpl(message::Message) override;

    Options opts_;
};


}  // namespace multio::action::interpolate
