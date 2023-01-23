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


namespace multio::action::interpolate {


/**
 * \class MultIO Action for interpolation/regridding
 */
class Interpolate final : public ChainedAction {
public:
    using ChainedAction::ChainedAction;

private:
    message::Message InterpolateInSinglePrecision(message::Message&&) const;
    message::Message InterpolateInDoublePrecision(message::Message&&) const;
    void print(std::ostream&) const override;
    void executeImpl(message::Message) const override;
};


}  // namespace multio::action::interpolate
