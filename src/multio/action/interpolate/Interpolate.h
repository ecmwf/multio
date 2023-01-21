/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/**
 * \file This file is used to define a special action needed to interpolate
 * fields.
 *
 *
 * @see Action.h
 * @see ChainedAction.h
 *
 * @author Mirco Valentini
 * @author Domokos Sarmany
 * @author Simon Smart
 * @author Tiago Quintino
 *
 * @date Nov 2022
 */


#pragma once

#include "multio/action/ChainedAction.h"


namespace multio {
namespace action {
namespace interpolate {


/**
 * \class Object used to wrap the MIR interpolation tool in order to enable
 *        interpolation between different grids in multio pipelines
 *
 * \todo  rework mir raw IO objects to allow cleaner code
 */
class Interpolate final : public ChainedAction {
private:
    /**
     * \brief Configuration for the interpolation object
     */
    const eckit::LocalConfiguration configurationContext_;

    int outputSize_;

    /**
     * \brief function used to perform an interpolation when
     * the message is raw; the template is used to perform the interpolation in sigle/double precision
     *
     * \param [in] msg input message
     * \param [in] mainConfiguration all the parameters/configuration needed to
     * perform a specific interpolation
     *
     * \see executeImpl
     * \see InterpolateRawMessageFloat
     */
    template <typename T>
    message::Message InterpolateRawMessage(message::Message&& msg) const;

    /**
     * \brief helper used to just log useful informations
     *
     * \param [inout] os output stream where to log infortmations
     */
    void print(std::ostream& os) const override;

public:
    /**
     * \brief constructor for the interpolation action
     *
     * \param [in] confCtx configuration parameters for the action
     */
    explicit Interpolate(const ConfigurationContext& confCtx);

    /**
     * \brief method used as high level wrapper for the interpolation library
     *
     * \param [in] msg perform the interpolation action
     *
     * \see InterpolateRawMessage
     *
     */
    void executeImpl(message::Message msg) const override;
};


}  // namespace interpolate
}  // namespace action
}  // namespace multio
