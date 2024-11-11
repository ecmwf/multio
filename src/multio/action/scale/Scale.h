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


namespace multio::action::scale {


/**
 * \class MultIO Action to scale fields
 */
class Scale final : public ChainedAction {
public:
    using ChainedAction::ChainedAction;

private:
    template <typename T>
    message::Message ScaleMessage(message::Message&&) const;
        // Template function to scale messages based on the Precision type
    template <typename T>
    message::Message ScaleMessage(message::Message&& msg) const;

    // Methods to scale data with and without missing values
    template <typename T>
    void scaleWithMissing(T* data, std::size_t size, double scalingFactor);

    template <typename T>
    void scaleWithoutMissing(T* data, std::size_t size, double scalingFactor);
    
    void print(std::ostream&) const override;
    void executeImpl(message::Message) override;
};


}  // namespace multio::action::scale
