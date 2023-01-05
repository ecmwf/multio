
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
 * \file This file is used to define construct the special parser used by the
 * interpolate actions. The parser is contructed according to the input data  in
 * the YAML configuration file.
 *
 * @see Interpolate.h
 * @see Action.h
 * @see ChainedAction.h
 * @see interpoalteParser.h
 * @see interpoalteParserAtlas.h
 * @see interpoalteParserPureForwarding.h
 *
 * @author Mirco Valentini
 * @author Domokos Sarmany
 * @author Simon Smart
 * @author Tiago Quintino
 *
 * @date Nov 2022
 */
#pragma once

// The parser interface
#include "InterpolateParser.h"

// All the available parser
#include "InterpolateParserAtlas.h"
#include "InterpolateParserPureForwarding.h"

// Namespace handling
namespace multio {
namespace action {
namespace interpolate {

/**
 * \class Singleton used to allocate the correct parser according to the input
 * pattern
 *
 * \todo improve the conditions to select the parser.
 */
class InterpolateParserFactory {
private:
    /**
     * \brief constructor of the factory
     */
    InterpolateParserFactory(){};

public:
    /**
     * \brief Get the insstance of hte factory
     *
     * \return the "single" instance of the factory
     */
    static InterpolateParserFactory& instance() {
        static InterpolateParserFactory singleton;
        return (singleton);
    };

    /**
     * \brief Methos used to effectively build the special parser
     *
     * \param [in] configurationContext tokenized YAML file from the
     * action configuration, used to select the correct parser.
     */
    ActionInterpolateHighParser* build(const eckit::LocalConfiguration& configurationContext) {
        ActionInterpolateHighParser* highParser;

        if (configurationContext.has("inputConfiguration") && configurationContext.has("jobConfiguration")
            && configurationContext.has("outputConfiguration") && configurationContext.has("outputSize")
            && !configurationContext.has("from") && !configurationContext.has("to")) {
            LOG_DEBUG_LIB(LibMultio) << "Interpolate Parser Factory :: allocate a "
                                        "\"Pure Forwarding\" parser"
                                     << std::endl
                                     << std::endl;
            // Construction of a pure forwarding parser
            highParser = new ActionInterpolateHighParserPureForwarding(configurationContext);
        }
        else if (!configurationContext.has("inputConfiguration") && !configurationContext.has("jobConfiguration")
                 && !configurationContext.has("outputConfiguration") && !configurationContext.has("outputSize")
                 && configurationContext.has("from") && configurationContext.has("to")) {
            // Construction of an Atlas parser
            LOG_DEBUG_LIB(LibMultio) << "Interpolate Parser Factory :: allocate an \"Atlas grids\" parser" << std::endl
                                     << std::endl;
            highParser = new ActionInterpolateHighParserAtlas(configurationContext);
        }
        else {
            throw eckit::NotImplemented("Action::Interpolate ::  Bad input sequence in YAML file",
                                        eckit::CodeLocation(__FILE__, __LINE__, __FUNCTION__));
        }
        // Exit point
        return (highParser);
    }
};

}  // namespace interpolate
}  // namespace action
}  // namespace multio
