/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Kevin Nobel

/// @date Aug 2025

#pragma once

#include "multio/action/ChainedAction.h"

namespace multio::action::average_rate {

namespace {
using Param2ParamMap = std::unordered_map<std::int64_t, std::int64_t>;
}

class AverageRate : public ChainedAction {
public:
    explicit AverageRate(const ComponentConfiguration& compConf);

    void executeImpl(message::Message msg) override;

private:
    template <typename Precision>
    void compute(message::Message& msg) const;

    void print(std::ostream&) const override;

    const Param2ParamMap paramMappings_;
};

}  // namespace multio::action::average_rate
