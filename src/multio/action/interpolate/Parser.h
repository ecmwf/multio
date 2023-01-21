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
 * \file This file is used to define a special parser for the configurng the
 * interpolate action. This particular parser is meant to give to the user the
 * full control of the input. No logic between the user input and the input to
 * the interpolator.
 *
 * @see Interpolate.h
 * @see Action.h
 * @see ChainedAction.h
 * @see interpoalteParser.h
 * @see interpoalteParserAtlas.h
 *
 * @author Mirco Valentini
 * @author Domokos Sarmany
 * @author Simon Smart
 * @author Tiago Quintino
 *
 * @date Nov 2022
 */

#pragma once

// System includes
#include <fstream>

//// Include all the mir headers (interpolation package)
#include "eckit/config/LocalConfiguration.h"
#include "eckit/value/Value.h"

#include "mir/api/MIRJob.h"
#include "mir/param/SimpleParametrisation.h"


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
        throw eckit::NotImplemented("Action::Interpolate :: Nested forwarding is not supported", Here());
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
            throw eckit::NotImplemented("Action::Interpolate :: Unsupported datatype", Here());
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
            throw eckit::NotImplemented("Action::Interpolate :: Unsupported datatype", Here());
        };
    }
};


/**
 * \class this class is used fill the configuration parameters for mir objects
 * (both input configuration and job configuration)
 */
template <typename DestinationType>
class Configurator {
private:
    /**
     * \brief YAML configuration of the object
     */
    const eckit::LocalConfiguration base_;

    /**
     * \brief Name of the field for the subconfiguration to be considered
     *        - "inputConfiguration"
     *        - "jobConfiguration"
     *        - "outputConfiguration"
     */
    const char* field_;

    /**
     * \brief Set configuration parameters from selected keys of input
     * metadata and fill the output object
     *
     * \param [in]   cfg configuration from YAML file
     * \param [in]   msg metadata of the input message
     * \param [inout] destination object where to forward configurations
     *
     * \todo reverse the keywords. It is more intuitive if to the left there is
     * the keyword in the mi object and to the right the keyword in the message
     * input metadata
     */
    void forwardFromInputMetadata(const eckit::LocalConfiguration& cfg, const message::Metadata& msg,
                                  DestinationType& destination) const {
        // Forward the selected keys to the output
        for (auto key : cfg.keys()) {
            if (msg.has(key)) {
                forwardConfiguration(msg, destination, key, cfg.getString(key));
            }
            else {
                std::ostringstream oss;
                oss << "Action::Interpolate::Parser :: Bad input sequence in YAML file -> " << key << std::endl;
                throw eckit::UserError(oss.str(), Here());
            }
        }
    };

    /**
     * \brief Forward all input metadata to the mir configuration object
     */
    void forwardAllInputMetadata(const message::Metadata& msg, DestinationType& destination) const {
        // Forward the selected keys to the output
        for (auto key : msg.keys()) {
            if (!destination.has(key)) {
                forwardConfiguration(msg, destination, key, key);
            }
            else {
                std::ostringstream oss;
                oss << "Action::Interpolate::Parser :: Key already present -> " << key << std::endl;
                throw eckit::UserError(oss.str(), Here());
            }
        }
    };

    /**
     * \brief Set configuration parameters from input metadata and fill the
     * output object
     *
     * \param [in]   cfg onfiguration from YAML file
     * \param [in]   msg metadata of the input message
     * \param [inout] destination object where to forward configurations
     */
    void setByValue(const eckit::LocalConfiguration& cfg, const message::Metadata& msg,
                    DestinationType& destination) const {
        // Loop over the keys to set the data into the destination container
        for (auto key : cfg.keys()) {
            forwardConfiguration(cfg, destination, key, key);
        }
    };

    /**
     * \brief Set configuration parameters from files.
     * A set of pairs of the form: <key>: <filename> is expected
     * In the templated destination object a pair name key with the contents
     * of the file is added. The file is expected to contain a single column
     * of integers. The number of lines is updated dynamically.
     *
     * \note At the moment all the data are considered to be \"long\" in order to
     * allow mir to recognize the pl vector.
     *
     * \param [in] cfg configuration from YAML file
     * \param [inout] destination object where to forward configurations
     */
    void loadFromFile(const eckit::LocalConfiguration& cfg, DestinationType& destination) const {
        for (auto key : cfg.keys()) {
            std::vector<long> tmp;
            readFile(cfg.getString(key), tmp);
            destination.set(key, tmp);
        }
    };

    /**
     * \brief Read values from an input file
     *
     * \param [in] fname name of the file to be read
     * \param [in] vec vector that will be filled with the file contents
     */
    void readFile(const std::string& fname, std::vector<long>& vec) const {
        std::fstream inputFile;
        vec.resize(0);
        inputFile.open(fname);
        if (inputFile.is_open()) {
            std::string str;
            while (getline(inputFile, str)) {
                vec.push_back(long(atoi(str.c_str())));
            }
            inputFile.close();  // close the file object.
        }
        else {
            std::ostringstream oss;
            oss << "Action::Interpolate::Parser :: Unable to open file -> " << fname << std::endl;
            throw eckit::UserError(oss.str(), Here());
        }
    };

public:
    /**
     * \brief Constructor for the object used as a parser for the YAML file
     *
     * \param [in] base configuration context of the action interpolate
     * \param [in] field name of the field to be read
     */
    explicit Configurator(const eckit::LocalConfiguration& base, const char* field) : base_(base), field_(field) {
        if (!base.has(field)) {
            std::ostringstream oss;
            oss << "Action::Interpolate::Parser :: Field not found in the input YAML file -> " << field << std::endl;
            throw eckit::UserError(oss.str(), Here());
        };
    };

    /**
     * \brief method used to forward the input configuration from the YAML
     * file to the mir configuration objects
     *
     * \param [in] inputMessageMetadata metadata of the input message
     * \param [out] Destination destination object where the configuration is
     * set (can be inputConfiguration, jobConfiguration, outputMetadata)
     */
    void UpdateConfiguration(const eckit::LocalConfiguration& inputMessageMetadata,
                             DestinationType& Destination) const {
        // Extract the configuration
        const eckit::LocalConfiguration cfg = base_.getSubConfiguration(field_);

        // Counter for the input configurations (used to detect empty
        // configurations)
        unsigned cnt = 0;

        // Forward only specific keywords from input metadata
        if (cfg.has("forwardFromInputMetadata")) {
            auto tmp = cfg.getSubConfiguration("forwardFromInputMetadata");
            forwardFromInputMetadata(tmp, inputMessageMetadata, Destination);
            cnt++;
        };

        // Forward all the input metadata to the destination
        if (cfg.has("forwardAllInputMetadata")) {
            forwardAllInputMetadata(inputMessageMetadata, Destination);
            cnt++;
        };

        // Load a configuration from file
        if (cfg.has("loadFromFile")) {
            auto tmp = cfg.getSubConfiguration("loadFromFile");
            loadFromFile(tmp, Destination);
            cnt++;
        };

        // Set configuration by key:value copy
        if (cfg.has("setByValue")) {
            auto tmp = cfg.getSubConfiguration("setByValue");
            setByValue(tmp, inputMessageMetadata, Destination);
            cnt++;
        };

        // Check for empty configuration
        if (!cfg.has("empty") && cnt == 0) {
            throw eckit::UserError(
                "Action::Interpolate::Parser :: unable to find a valid configuration in the input file", Here());
        };
    };
};

/**
 * \class this class is used to handle the configuration for an
 * interpolate Action.
 */
class ActionInterpolateHighParserPureForwarding {
private:
    /**
     * \brief configuration context for the entire mission
     */
    const eckit::LocalConfiguration configurationContext_;

    /**
     * \brief input parameters of the interplator
     */
    const Configurator<mir::param::SimpleParametrisation> input_;

    /**
     * \brief configuration parameters of the interplator
     */
    const Configurator<mir::api::MIRJob> job_;

    /**
     * \brief output parameters of the interplator
     */
    const Configurator<eckit::LocalConfiguration> output_;

    /**
     * \brief expected size of the output message
     */
    int outputSize_;

public:
    /**
     * \brief Constructor of the class. It is meant to be constructed only by
     * the HighParser during the parsing of the input YAML file.
     *
     * \param [in] missionConfigurationContext tokenized YAML file from the
     * action
     */
    explicit ActionInterpolateHighParserPureForwarding(const eckit::LocalConfiguration& configurationContext) :
        configurationContext_(configurationContext),
        input_(configurationContext, "inputConfiguration"),
        job_(configurationContext, "jobConfiguration"),
        output_(configurationContext, "outputConfiguration"),
        outputSize_(configurationContext.getLong("outputSize")) {
    };

    /**
     * \brief Get the expected dimension of the output field
     *
     * \return expected size of the output fields
     */
    int outputSize() const { return (outputSize_); };

    /**
     * \brief Fill all the configuration parameters into the input
     * parameters for mir interpolation object
     *
     * \param [in]    inputMessageMetadata metadata of the input message
     * \param [inout] mirInputParams mir input parameters to be filled
     */
    void MIRInput(const eckit::LocalConfiguration& inputMessageMetadata,
                  mir::param::SimpleParametrisation& mirInputParams) const {
        // Fill the MIRinput parameters
        input_.UpdateConfiguration(inputMessageMetadata, mirInputParams);
    };

    /**
     * \brief Fill all the configuration parameters into the output
     * mir interpolation object
     *
     * \param [in]    inputMessageMetadata metadata of the input message
     * \param [inout] outputMetadata mir input parameters to be filled
     */
    void MIROutput(const eckit::LocalConfiguration& inputMessageMetadata,
                   eckit::LocalConfiguration& outputMetadata) const {
        // Fill the MIRoutput parameters
        output_.UpdateConfiguration(inputMessageMetadata, outputMetadata);
    };

    /**
     * \brief Fill all the configuration parameters into the mir interpolation
     * object
     *
     * \param [in]    inputMessageMetadata metadata of the input message
     * \param [inout] mitInputParams mir input parameters to be filled
     */
    void MIRJob(const eckit::LocalConfiguration& inputMessageMetadata, mir::api::MIRJob& mirJobParams) const {
        // Fill the MIRjob parameters
        job_.UpdateConfiguration(inputMessageMetadata, mirJobParams);
    };
};

}  // namespace interpolate
}  // namespace action
}  // namespace multio
