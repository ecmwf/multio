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

#include <iomanip>
#include <map>
#include <memory>
#include <sstream>
#include <string>
#include <vector>

#include "FesomInterpolationWeights.h"
#include "InterpolateFESOM_debug.h"
#include "atlas_io/atlas-io.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"
#include "multio/LibMultio.h"
#include "multio/action/ChainedAction.h"

namespace multio::action::interpolateFESOM {

template <typename MatrixType, typename = std::enable_if_t<std::is_floating_point<MatrixType>::value>>
class Fesom2HEALPix {
private:
    size_t nnz_;
    size_t nRows_;
    size_t nCols_;
    size_t nOutRows_;
    std::vector<std::int32_t> landSeaMask_;
    std::vector<std::int32_t> rowStart_;
    std::vector<std::int32_t> colIdx_;
    std::vector<MatrixType> values_;


    std::string generateCacheFileName(const std::string& cachePath,
                                      const std::string& fesomGridName, const std::string& domain, size_t NSide,
                                      orderingConvention_e orderingConvention, double level) {
        INTERPOLATE_FESOM_OUT_STREAM << " - Fesom2HEALPix: enter generate cache file name" << std::endl;
        std::ostringstream os;
        os << cachePath << "/"
           << fesomCacheName( fesomGridName, domain, (sizeof(MatrixType) == 4 ? "single" : "double"), NSide, orderingConvention, level )
           << ".atlas";
        std::string fname{os.str()};
        INTERPOLATE_FESOM_OUT_STREAM << " - Reading file: " << fname << std::endl;
        eckit::PathName file{fname};
        if (!file.exists()) {
            throw eckit::SeriousBug("Unable to open file: " + fname, Here());
        }
        INTERPOLATE_FESOM_OUT_STREAM << " - Fesom2HEALPix: exit generate cache file name" << std::endl;
        return fname;
    }

public:
    Fesom2HEALPix(const message::Message& msg, const std::string& cachePath, const std::string& fesomGridName,
                  size_t NSide, orderingConvention_e orderingConvention) {
        INTERPOLATE_FESOM_OUT_STREAM << " - Fesom2HEALPix: enter file cache constructor" << std::endl;
        // Generate cache file name
        size_t level = static_cast<size_t>(msg.metadata().getLong("level", msg.metadata().getDouble("levelist", 0)));
        if ( (msg.metadata().getString("category" )       == "ocean-3d") && 
             (msg.metadata().getString("fesomLevelType" ) == "level") ) {
            if (level == 0) {
                std::ostringstream os;
                os << " - Wrong level for the oceal level: " << std::endl;
                throw eckit::SeriousBug(os.str(), Here());
            }        
            level--;    
        }
        const std::string domain = msg.metadata().getString("domain" );
        std::string file = generateCacheFileName(cachePath, fesomGridName, domain, NSide, orderingConvention, level);
        // TODO: Here with some flag it is possible to add the possibility
        // to generate the cache in some way
        // Initialize from atlasIO cache
        size_t version;
        size_t NSideR;
        size_t levelR;
        atlas::io::RecordReader reader(file);
        // Read the objects needed for the interpolation
        reader.read("version", version);
        reader.wait();
        if (version != 0) {
            std::ostringstream os;
            os << "Wrong version: " << version << " " << 0 << std::endl;
            throw eckit::SeriousBug(os.str(), Here());
        }
        reader.read("nside", NSideR);
        reader.read("level", levelR);
        reader.read("nnz", nnz_);
        reader.read("nRows", nRows_);
        reader.read("nCols", nCols_);
        reader.read("nOutRows", nOutRows_);
        reader.read("landSeaMask", landSeaMask_);
        reader.read("rowPtr", rowStart_);
        reader.read("colIdx", colIdx_);
        reader.read("weights", values_);
        reader.wait();
        // Check sizes
        if (landSeaMask_.size() != nRows_) {
            std::ostringstream os;
            os << " - Wrong size of lenad sea mask: " << landSeaMask_.size() << " " << nRows_ << std::endl;
            throw eckit::SeriousBug(os.str(), Here());
        }
        if (rowStart_.size() != (nRows_ + 1)) {
            std::ostringstream os;
            os << " - Wrong size of rowstart: " << rowStart_.size() << " " << (nRows_ + 1) << std::endl;
            throw eckit::SeriousBug(os.str(), Here());
        }
        if (colIdx_.size() != nnz_) {
            std::ostringstream os;
            os << " - Wrong size of colidx: " << colIdx_.size() << " " << nnz_ << std::endl;
            throw eckit::SeriousBug(os.str(), Here());
        }
        if (values_.size() != nnz_) {
            std::ostringstream os;
            os << " - Wrong size of values: " << values_.size() << " " << nnz_ << std::endl;
            throw eckit::SeriousBug(os.str(), Here());
        }
        if (NSideR != NSide) {
            std::ostringstream os;
            os << " - Wrng NSide: " << NSideR << " " << NSide << std::endl;
            throw eckit::SeriousBug(os.str(), Here());
        }
        if (std::fabs(levelR - level) > 1.0E-12) {
            std::ostringstream os;
            os << " - Wrong level: " << levelR << " " << level << std::endl;
            throw eckit::SeriousBug(os.str(), Here());
        }
        INTERPOLATE_FESOM_OUT_STREAM << " - Fesom2HEALPix: exit file cache constructor" << std::endl;
        // Exit point
        return;
    }


    template <typename InFieldType, typename OutFieldType>
    void interpolate(const InFieldType* fesomField, OutFieldType* HEALPixField, size_t inputSize, size_t outputSize,
                     OutFieldType missingValue) {
        INTERPOLATE_FESOM_OUT_STREAM << " - Fesom2HEALPix: enter intrpolate" << std::endl;
        // Check sizes
        // @note: Due to topography the input size can be bigger than the expected number of columns!!!
        if (inputSize < nCols_) {
            std::ostringstream os;
            os << " - Wrong input size: " << inputSize << " " << nCols_ << std::endl;
            throw eckit::SeriousBug(os.str(), Here());
        }
        if (outputSize != nOutRows_) {
            std::ostringstream os;
            os << " - Wrong output size: " << outputSize << " " << nOutRows_ << std::endl;
            throw eckit::SeriousBug(os.str(), Here());
        }
        INTERPOLATE_FESOM_OUT_STREAM << " - intrpolate: initialize to missing values" << std::endl;
        // Initialize output field
        for (size_t i = 0; i < nOutRows_; i++) {
            HEALPixField[i] = missingValue;
        }
        INTERPOLATE_FESOM_OUT_STREAM << " - intrpolate: do interpolation " << std::endl;
        // Perform the interpolation
        for (size_t iRow = 0; iRow < nRows_; iRow++) {
            size_t outIdx = landSeaMask_[iRow];
            HEALPixField[outIdx] = 0.0;
            for (size_t colPtr = rowStart_[iRow]; colPtr < rowStart_[iRow + 1]; colPtr++) {
                size_t iCol = colIdx_[colPtr];
                OutFieldType weight = static_cast<OutFieldType>(values_[colPtr]);
                OutFieldType inpVal = static_cast<OutFieldType>(fesomField[iCol]);
                HEALPixField[outIdx] += weight * inpVal;
            }
        }
        INTERPOLATE_FESOM_OUT_STREAM << " - Fesom2HEALPix: exit intrpolate" << std::endl;
        // Exit point
        return;
    }
};

/**
 * \class MultIO Action for interpolation/regridding from fesom grids to HEALPix
 */
template <typename T>
class InterpolateFESOM final : public ChainedAction {
public:
    using ChainedAction::ChainedAction;
    explicit InterpolateFESOM(const ComponentConfiguration& compConf);

private:
    void print(std::ostream&) const override;
    void executeImpl(message::Message) override;
    std::string generateKey(const message::Message& msg) const;
    // Fesom interpolators with at different levels (different LSM)
    const size_t NSide_;
    const orderingConvention_e orderingConvention_;
    const T missingValue_;
    const std::string outputPrecision_;
    const std::string cachePath_;
    // const std::string fesomGridName_;
    // FesomInterpolationWeights cacheGenerator_;
    std::map<std::string, std::unique_ptr<Fesom2HEALPix<T>>> Interpolators_;
};


}  // namespace multio::action::interpolateFESOM
