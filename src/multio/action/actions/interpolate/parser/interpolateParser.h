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
 * \file This file is used to define an abstract parser for the configuration of
 * the interpolate action. This abstraction is needed in order to allow
 * different input patterns.
 *
 *
 * @see Action.h
 * @see Aggregation.h
 * @see Encode.h
 *
 * @author Mirco Valentini
 * @author Domokos Sarmany
 * @author Simon Smart
 * @author Tiago Quintino
 *
 * @date Nov 2022
 */

#ifndef multio_action_interpolateParser_H
#define multio_action_interpolateParser_H

// Include all the mir headers (interpolation package)
#include "eckit/value/Value.h"
#include "mir/api/MIRJob.h"
#include "mir/input/RawInput.h"
#include "mir/output/RawOutput.h"

// Loggin utilities
#include "multio/LibMultio.h"

// Namespace handling
namespace multio {
namespace action {
namespace interpolate {

/**
 * \brief forwarder for the input data. The destination object is assumed
 * to have only an overloaded set method
 *
 * \note bad implementation and far from beeing general, but it works for now
 *
 * \param [in] Source source object where to read data
 * \param [inout] Destination bject where to forward configurations
 * \param [in] keySource key in the source object
 * \param [in] keyDestination key in the destination object
 */
template <typename DestinationType>
void forwardConfiguration(const eckit::LocalConfiguration& Source, DestinationType& Destination,
                          const std::string& keySource, const std::string& keyDestination) {
    eckit::Value cfgVal = Source.getSubConfiguration(keySource).get();
    eckit::LocalConfiguration tmp;
    if (cfgVal.isMap()) {
        std::ostringstream oss;
        oss << "ERROR :: Action::Interpolate :: Nested forwarding is not supported" << std::endl
            << "    file.....: " << __FILE__ << std::endl
            << "    function.: " << __FUNCTION__ << std::endl
            << "    line.....: " << __LINE__ << std::endl
            << std::endl;
        throw eckit::SeriousBug(oss.str());
    }
    else if (cfgVal.isList()) {
        if (cfgVal.head().isDouble()) {
            Destination.set(keyDestination, Source.getDoubleVector(keySource));
        }
        else if (cfgVal.head().isNumber()) {
            // Bad special case needed for handle the pl array in atlas grids
            Destination.set(keyDestination, Source.getLongVector(keySource));
        }
        else if (cfgVal.head().isString()) {
            Destination.set(keyDestination, Source.getStringVector(keySource));
        }
        else {
            std::ostringstream oss;
            oss << "ERROR :: Action::Interpolate :: Unsupported datatype" << std::endl
                << "    file.....: " << __FILE__ << std::endl
                << "    function.: " << __FUNCTION__ << std::endl
                << "    line.....: " << __LINE__ << std::endl
                << std::endl;
            throw eckit::SeriousBug(oss.str());
        };
    }
    else {
        if (cfgVal.isBool()) {
            Destination.set(keyDestination, Source.getBool(keySource));
        }
        else if (cfgVal.isDouble()) {
            Destination.set(keyDestination, Source.getDouble(keySource));
        }
        else if (cfgVal.isNumber()) {
            Destination.set(keyDestination, Source.getInt(keySource));
        }
        else if (cfgVal.isString()) {
            Destination.set(keyDestination, Source.getString(keySource).c_str());
        }
        else {
            std::ostringstream oss;
            oss << "ERROR :: Action::Interpolate :: Unsupported datatype" << std::endl
                << "    file.....: " << __FILE__ << std::endl
                << "    function.: " << __FUNCTION__ << std::endl
                << "    line.....: " << __LINE__ << std::endl
                << std::endl;
            throw eckit::SeriousBug(oss.str());
        };
    }
};

/**
 * \class this class is used to handle the configuration for an
 * interpolate Action.
 *
 * \see Interpolate
 */
class ActionInterpolateHighParser {
protected:
    /**
     * \brief configuration context for the entire mission
     */
    const eckit::LocalConfiguration configurationContext_;

public:
    /**
     * \brief Constructor of the class. It is meant to be constructed only by
     * the HighParser during the parsing of the input YAML file.
     *
     * \param [in] missionConfigurationContext tokenized YAML file from the
     * action
     */
    explicit ActionInterpolateHighParser(const eckit::LocalConfiguration& configurationContext) :
        configurationContext_(configurationContext){};

    /**
     * \brief Call inherited destructors
     */
    virtual ~ActionInterpolateHighParser() { return; };

    /**
     * \brief Get the expected dimension of the output field
     *
     * \return expected size of the output fields
     */
    virtual int outputSize() const = 0;

    /**
     * \brief Fill all the configuration parameters into the input
     * parameters for mir interpolation object
     *
     * \param [in]    inputMessageMetadata metadata of the input message
     * \param [inout] mirInputParams mir input parameters to be filled
     */
    virtual void MIRInput(const eckit::LocalConfiguration& inputMessageMetadata,
                          mir::param::SimpleParametrisation& mirInputParams) const
        = 0;

    /**
     * \brief Fill all the configuration parameters into the output
     * parameters for mir interpolation object
     *
     * \param [in]    inputMessageMetadata metadata of the input message
     * \param [inout] outputMetadata mir input parameters to be filled
     */
    virtual void MIROutput(const eckit::LocalConfiguration& inputMessageMetadata,
                           eckit::LocalConfiguration& outputMetadata) const
        = 0;

    /**
     * \brief Fill all the configuration parameters into the mir interpolation
     * object
     *
     * \param [in]    inputMessageMetadata metadata of the input message
     * \param [inout] mitInputParams mir input parameters to be filled
     */
    virtual void MIRJob(const eckit::LocalConfiguration& inputMessageMetadata, mir::api::MIRJob& mirJobParams) = 0;
};

}  // namespace interpolate
}  // namespace action
}  // namespace multio

#endif  // multio_action_interpolateParser_H