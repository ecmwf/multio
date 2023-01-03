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
 * interpolate action. This particular parser is meant to use the predefined
 * atlas grid as input and output for the interpolato. In this way the user does
 * not need to configure all the parameters.
 *
 *
 * @see Interpolate.h
 * @see Action.h
 * @see ChainedAction.h
 * @see interpoalteParser.h
 * @see interpoalteParserPureForwarding.h
 *
 * @author Mirco Valentini
 * @author Domokos Sarmany
 * @author Simon Smart
 * @author Tiago Quintino
 *
 * @date Nov 2022
 */

#ifndef multio_action_interpolateParserAtlas_H
#define multio_action_interpolateParserAtlas_H

// System includes
#include <cctype>
#include <cstdlib>
#include <fstream>
#include <string>

// Include the abstract parser class
#include "InterpolateParser.h"

// Include all the mir headers (interpolation package)
#include "eckit/value/Value.h"
#include "mir/api/MIRJob.h"
#include "mir/input/RawInput.h"
#include "mir/output/RawOutput.h"

// include headers from atlas in order to simplify the grid input
#include "atlas/grid.h"
#include "atlas/grid/detail/grid/GridBuilder.h"
#include "atlas/grid/detail/grid/GridFactory.h"
#include "atlas/projection/detail/ProjectionFactory.h"
#include "atlas/runtime/AtlasTool.h"
#include "atlas/util/NormaliseLongitude.h"

// Namespace handling
namespace multio {
namespace action {
namespace interpolate {

/**
 * \class this class is used to handle the configuration for an interpolate
 * action. In particular specifica parameter that describe the grids are
 * extracted from the atlas database.
 *
 * \note a special grid configuration (for spherica harmonics) has been added in
 * order to reproduce all the needed test cases. The grammar for this
 * configuration is SH<trunc>, where trunc is the number of harmonics
 */
class ActionInterpolateHighParserAtlas final : public ActionInterpolateHighParser {
private:
    /**
     * \brief atlas codename of the input grid
     *
     * \note see "./atlas-grid --list" for a list of valid atlas codenames.
     * \note only a tiny subset of valid atlas codenames is handled by this class
     */
    std::string inputGridName_;

    /**
     * \brief map that contains all the input configurations needed for the
     * description of the input field
     */
    eckit::LocalConfiguration mirInputParams_;

    /**
     * \brief atlas codename of the output grid
     *
     * \note see "./atlas-grid --list" for a list of valid atlas codenames.
     * \note only a tiny subset of valid atlas codenames is handled by this class
     */
    std::string outputGridName_;

    /**
     * \brief map that contains all the input configurations needed for the
     * description of the output field
     */
    eckit::LocalConfiguration mirJobParams_;

    /**
     * \brief Size of the input grid (number of nodes)
     */
    long inputSize_;

    /**
     * \brief Size of the output grid (number of nodes)
     */
    long outputSize_;

    /**
     * \brief Special parser for treating the spherical harmonics not present in
     * atlas. The grammar is supposed to be: SH[1-9][0-9]*
     *
     * \param str [in] name of the grid (passed in the YAML file)
     * \param N   [out] number of modes for the spherical harmonics
     *
     * \return true if the input string "str" is a valid name for a spherical
     * harmonics field
     */
    bool sphericalHarmonics(const std::string& str, int& N) {
        // initialization
        N = 0;
        const char* tmp = str.c_str();
        bool status = false;
        // If str is shorter than 3 characters then it is not a valid spherical
        // harmonics
        if (str.size() > 2) {
            // Check that the first two characters are "SH"
            if (tmp[0] == 'S' && tmp[1] == 'H') {
                // Check that after "SH" all other characters in the string are digits
                bool check = true;
                for (int i = 2; i < str.length(); ++i) {
                    if (isdigit(tmp[i]) == 0) {
                        check = false;
                    }
                }
                // First digit needs to be different from zero
                if (check && tmp[2] == '0') {
                    check = false;
                    status = false;
                    N = -1;
                }
                // If all checks are ok then convert the digits in a number and set the
                // status to "success"
                if (check) {
                    N = atoi(&tmp[2]);
                    status = true;
                }
                else {
                    // The characters after "SH" are not all digits
                    N = -2;
                    status = false;
                }
            }
            else {
                // Firsst two characters are not "SH"
                N = -3;
                status = false;
            }
        }
        else {
            // String is not longer than two characters
            N = -4;
            status = false;
        }
        // Exit point
        return (status);
    };

    /**
     * \brief Code needed to parse the source grid. The source grid can be a
     * spherical harmonics or a valid atlas grid. This function has two main
     * targets:
     *  - filling the mirInputParams_
     *  - filling the inputGridName_
     *
     * The logic of the function is the following: first check if it is a
     * spherical harmonics; if not check if it is in the valid subset of the atlas
     * grids handled by this object.
     *
     * Valid atlas grids for this objec are:
     * - reducedGG grids, defined with the grammar O[1-9][0-9*]
     * - regularLL grids, defined with the grammar L[1-9][0-9*]x[1-9][0-9]*
     *
     * All other codenames will generate errors
     *
     */
    void parseSourceGrid() {
        // Local variables
        eckit::Value cfgVal;
        // Read the input grid
        cfgVal = configurationContext_.getSubConfiguration("from").get();
        if (cfgVal.isString()) {
            //
            inputGridName_ = configurationContext_.getString("from");
            int N = 0;
            if (sphericalHarmonics(inputGridName_, N)) {
                // At first check if the grid is spherical harmonics. Grammar to set a
                // sh grid: SH-<Truncation>
                mirInputParams_.set("gridType", "sh");
                mirInputParams_.set("spectral", true);
                mirInputParams_.set("truncation", N);
                inputSize_ = (N + 1) * (N + 2);
            }
            else {
                // If it is not a spherical harmonics, try to match an atlas grid
                auto inputGrid = atlas::Grid(inputGridName_);
                if (!inputGrid) {
                    std::ostringstream oss;
                    oss << "ERROR :: Action::Interpolate::Parser :: invalid input grid "
                           "-> "
                        << inputGridName_ << std::endl
                        << "    file.....: " << __FILE__ << std::endl
                        << "    function.: " << __FUNCTION__ << std::endl
                        << "    line.....: " << __LINE__ << std::endl
                        << std::endl;
                    throw eckit::SeriousBug(oss.str());
                }
                else {
                    // Check if the grid is a reduced gaussian grid
                    auto gaussianGrid = atlas::GaussianGrid(inputGrid);
                    auto structuredGrid = atlas::StructuredGrid(inputGrid);
                    if (gaussianGrid) {
                        if (gaussianGrid.reduced()) {
                            auto reducedGG = atlas::ReducedGaussianGrid(inputGrid);
                            auto tmp = reducedGG.nx();
                            std::vector<long> pl(tmp.size(), 0);
                            for (int i = 0; i < tmp.size(); ++i) {
                                pl[i] = long(tmp[i]);
                            }
                            mirInputParams_.set("gridType", "reduced_gg");
                            mirInputParams_.set("gridded", true);
                            mirInputParams_.set("pl", pl);
                            mirInputParams_.set("N", gaussianGrid.N());
                            if (auto bb = inputGrid.lonlatBoundingBox()) {
                                mirInputParams_.set("north", bb.north());
                                mirInputParams_.set("south", bb.south());
                                mirInputParams_.set("east", bb.east());
                                mirInputParams_.set("west", bb.west());
                            }
                            else {
                                std::ostringstream oss;
                                oss << "ERROR :: Action::Interpolate::Parser :: Unable to get "
                                       "the bounding box "
                                    << std::endl
                                    << "    file.....: " << __FILE__ << std::endl
                                    << "    function.: " << __FUNCTION__ << std::endl
                                    << "    line.....: " << __LINE__ << std::endl
                                    << std::endl;
                                throw eckit::SeriousBug(oss.str());
                            }
                            inputSize_ = inputGrid.size();
                        }
                        else {
                            std::ostringstream oss;
                            oss << "ERROR :: Action::Interpolate::Parser :: non-reduced "
                                   "Gaussian grids are still not supported -> "
                                << inputGridName_ << std::endl
                                << "    file.....: " << __FILE__ << std::endl
                                << "    function.: " << __FUNCTION__ << std::endl
                                << "    line.....: " << __LINE__ << std::endl
                                << std::endl;
                            throw eckit::SeriousBug(oss.str());
                        }
                    }
                    else if (structuredGrid) {
                        mirInputParams_.set("gridType", "regular_ll");
                        mirInputParams_.set("gridded", true);
                        // Get the bounding box of hte grid
                        if (auto bb = inputGrid.lonlatBoundingBox()) {
                            mirInputParams_.set("north", bb.north());
                            mirInputParams_.set("south", bb.south());
                            mirInputParams_.set("east", bb.east());
                            mirInputParams_.set("west", bb.west());
                        }
                        else {
                            std::ostringstream oss;
                            oss << "ERROR :: Action::Interpolate::Parser :: Unable to get "
                                   "the bounding box "
                                << std::endl
                                << "    file.....: " << __FILE__ << std::endl
                                << "    function.: " << __FUNCTION__ << std::endl
                                << "    line.....: " << __LINE__ << std::endl
                                << std::endl;
                            throw eckit::SeriousBug(oss.str());
                        }
                        double degSN;
                        double degWE;
                        // Compute the grid resolution lat/lon
                        // (Code taken from atlas-grids.cc)
                        degSN = (structuredGrid.y().front() - structuredGrid.y().back()) / (structuredGrid.ny() - 1);
                        degWE = 360. / static_cast<double>(structuredGrid.nx(structuredGrid.ny() / 2));
                        // Set the mir parameters
                        mirInputParams_.set("south_north_increment", degSN);
                        mirInputParams_.set("west_east_increment", degWE);
                        mirInputParams_.set("Ni", structuredGrid.nxmax());
                        mirInputParams_.set("Nj", structuredGrid.ny());
                        // Compute the number of pointes in the grid
                        inputSize_ = inputGrid.size();
                    }
                    else {
                        std::ostringstream oss;
                        oss << "ERROR :: Action::Interpolate::Parser :: grid still not "
                               "supported -> "
                            << inputGridName_ << std::endl
                            << "    file.....: " << __FILE__ << std::endl
                            << "    function.: " << __FUNCTION__ << std::endl
                            << "    line.....: " << __LINE__ << std::endl
                            << std::endl;
                        throw eckit::SeriousBug(oss.str());
                    }
                }
            }
        }
        else {
            std::ostringstream oss;
            oss << "ERROR :: Action::Interpolate::Parser :: Input grid name must be "
                   "a string"
                << std::endl
                << "    file.....: " << __FILE__ << std::endl
                << "    function.: " << __FUNCTION__ << std::endl
                << "    line.....: " << __LINE__ << std::endl
                << std::endl;
            throw eckit::SeriousBug(oss.str());
        };
        // Exit point
        return;
    };

    /**
     * \brief Code needed to parse the target grid. The target grid can be a
     * a valid atlas grid or (the same name of the input grid in case of simple
     * copy). This function has two main targets:
     *  - filling the mirJobParams_
     *  - filling the outputGridName_
     *
     * The logic of the function is the following: first check if it is a
     * simple copy, in this case we need only the size; if not check if it is in
     * the valid subset of the atlas grids handled by this object.
     *
     * Valid atlas grids for this objec are:
     * - regularLL grids, defined with the grammar L[1-9][0-9*]x[1-9][0-9]*
     *
     * All other codenames will generate errors
     *
     */
    void parseTargetGrid() {
        // Local variables
        eckit::Value cfgVal;
        // Read the input grid
        cfgVal = configurationContext_.getSubConfiguration("to").get();
        if (cfgVal.isString()) {
            //
            outputGridName_ = configurationContext_.getString("to");
            // If the output name is the same as the input name then the interpolation
            // is just a copy, and we need only the size to allocate the buffer
            if (inputGridName_.compare(outputGridName_) == 0) {
                outputSize_ = inputSize_;
            }
            else {
                // If it is not a copy, try to match an valid atlas grid
                auto outputGrid = atlas::Grid(outputGridName_);
                if (!outputGrid) {
                    std::ostringstream oss;
                    oss << "ERROR :: Action::Interpolate::Parser :: invalid output grid "
                           "-> "
                        << outputGridName_ << std::endl
                        << "    file.....: " << __FILE__ << std::endl
                        << "    function.: " << __FUNCTION__ << std::endl
                        << "    line.....: " << __LINE__ << std::endl
                        << std::endl;
                    throw eckit::SeriousBug(oss.str());
                }
                else {
                    // Check if the grid is a reduced gaussian grid
                    auto structuredGrid = atlas::StructuredGrid(outputGrid);
                    if (structuredGrid) {
                        // mirJobParams_.set("gridType", "regular_ll");
                        // mirJobParams_.set("gridded", true);
                        // Get the bounding box of hte grid
                        if (auto bb = outputGrid.lonlatBoundingBox()) {
                            std::vector<double> tmp(4);
                            tmp[0] = bb.north();
                            tmp[1] = bb.west();
                            tmp[2] = bb.south();
                            tmp[3] = bb.east();
                            mirJobParams_.set("area", tmp);
                        }
                        else {
                            std::ostringstream oss;
                            oss << "ERROR :: Action::Interpolate::Parser :: Unable to get "
                                   "the bounding box "
                                << std::endl
                                << "    file.....: " << __FILE__ << std::endl
                                << "    function.: " << __FUNCTION__ << std::endl
                                << "    line.....: " << __LINE__ << std::endl
                                << std::endl;
                            throw eckit::SeriousBug(oss.str());
                        }
                        double degSN;
                        double degWE;
                        // Compute the grid resolution
                        degSN = (structuredGrid.y().front() - structuredGrid.y().back()) / (structuredGrid.ny() - 1);
                        degWE = 360. / static_cast<double>(structuredGrid.nx(structuredGrid.ny() / 2));
                        // Compute
                        std::vector<double> tmp(2);
                        tmp[0] = degSN;
                        tmp[1] = degWE;
                        mirJobParams_.set("grid", tmp);
                        mirJobParams_.set("interpolation", "linear");
                        mirJobParams_.set("caching", true);
                        // Compute the number of pointes in the grid
                        outputSize_ = outputGrid.size();
                    }
                    else {
                        std::ostringstream oss;
                        oss << "ERROR :: Action::Interpolate::Parser :: grid still not "
                               "supported -> "
                            << outputGridName_ << std::endl
                            << "    file.....: " << __FILE__ << std::endl
                            << "    function.: " << __FUNCTION__ << std::endl
                            << "    line.....: " << __LINE__ << std::endl
                            << std::endl;
                        throw eckit::SeriousBug(oss.str());
                    }
                }
            }
        }
        else {
            std::ostringstream oss;
            oss << "ERROR :: Action::Interpolate::Parser :: Output grid name must be "
                   "a string"
                << std::endl
                << "    file.....: " << __FILE__ << std::endl
                << "    function.: " << __FUNCTION__ << std::endl
                << "    line.....: " << __LINE__ << std::endl
                << std::endl;
            throw eckit::SeriousBug(oss.str());
        };
        // Exit point
        return;
    };

public:
    /**
     * \brief Constructor of the class. It is meant to be constructed only by
     * the HighParser during the parsing of the input YAML file.
     *
     * \param [in] missionConfigurationContext tokenized YAML file from the
     * action
     */
    explicit ActionInterpolateHighParserAtlas(const eckit::LocalConfiguration& configurationContext) :
        ActionInterpolateHighParser(configurationContext), inputSize_(0), outputSize_(0) {
        // Source grid
        parseSourceGrid();
        // Target grid;
        parseTargetGrid();
        // Exit point
        return;
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
        for (auto key : mirInputParams_.keys()) {
            forwardConfiguration(mirInputParams_, mirInputParams, key, key);
        }
    };

    /**
     * \brief Fill all the configuration parameters into the output
     * parameters for mir interpolation object
     *
     * \param [in]    inputMessageMetadata metadata of the input message
     * \param [inout] outputMetadata mir input parameters to be filled
     */
    void MIROutput(const eckit::LocalConfiguration& inputMessageMetadata,
                   eckit::LocalConfiguration& outputMetadata) const {
        // Nothing to do
        return;
    };

    /**
     * \brief Fill all the configuration parameters into the mir interpolation
     * object
     *
     * \param [in]    inputMessageMetadata metadata of the input message
     * \param [inout] mitInputParams mir input parameters to be filled
     */
    void MIRJob(const eckit::LocalConfiguration& inputMessageMetadata, mir::api::MIRJob& mirJobParams) {
        // Force mir copy when output is equal to input
        if (inputGridName_.compare(outputGridName_) != 0) {
            for (auto key : mirJobParams_.keys()) {
                forwardConfiguration(mirJobParams_, mirJobParams, key, key);
            }
        }
    };
};

}  // namespace interpolate
}  // namespace action
}  // namespace multio

#endif