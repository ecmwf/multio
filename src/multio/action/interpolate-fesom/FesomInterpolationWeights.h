
#pragma once

#include <algorithm>
#include <cmath>
#include <map>
#include <vector>

#include "InterpolateFesom_debug.h"
#include "eckit/codec/codec.h"
#include "eckit/exception/Exceptions.h"
#include "multio/LibMultio.h"

#define nside2npix(NSIDE) (NSIDE * NSIDE * 12)

namespace multio::action::interpolate_fesom {

enum class orderingConvention_e : unsigned int
{
    RING,
    NESTED,
    UNKNOWN
};

orderingConvention_e orderingConvention_string2enum(const std::string& orderingConvention);
std::string orderingConvention_enum2string(orderingConvention_e orderingConvention);


std::string fesomCacheName(const std::string& fesomName, const std::string& domain, const std::string& precision,
                           size_t NSide, orderingConvention_e orderingConvention, double level);

class Tri {
private:
    std::int32_t i_;  // Index in the HEALPix grid
    std::int32_t j_;  // Index in the FESOM grid
    double v_;        // Value of the weight

public:
    Tri();
    Tri(std::int32_t i, std::int32_t j, double v);

    std::size_t h() const;
    std::size_t f() const;
    double v() const;
    std::size_t idx(std::size_t scale) const;
    std::size_t reverse_idx(std::size_t scale) const;
    void print() const;
};


class FesomInterpolationWeights {
private:
    bool initialized_;

    std::vector<Tri> triplets_;


    void clearTriplets();
    void sortTriplets();
    void reverse_sortTriplets();


    template <typename T>
    void triplets2CSR(size_t NSide, size_t level, size_t& nnz, size_t& nRows, size_t& nCols, size_t& nOutRows,
                      std::vector<std::int32_t>& landSeaMask, std::vector<std::int32_t>& rowStart,
                      std::vector<std::int32_t>& colIdx, std::vector<T>& values) const {

        INTERPOLATE_FESOM_OUT_STREAM << " - FesomIntermopationWeights: enter triplets2CSR"
                                     << (sizeof(T) == 4 ? "<single>" : "<double>") << std::endl;

        if (!initialized_) {
            std::ostringstream os;
            os << "Object not initialized" << std::endl;
            throw eckit::SeriousBug(os.str(), Here());
        }

        nOutRows = nside2npix(NSide);
        nCols = 0;
        nRows = 0;
        nnz = triplets_.size();
        size_t cnt = 0;
        size_t curr;
        size_t prev = 1000000000;
        landSeaMask.resize(0);
        rowStart.resize(0);

        colIdx.resize(nnz);
        values.resize(nnz);
        for (const auto& t : triplets_) {
            colIdx[cnt] = t.f();
            values[cnt] = static_cast<T>(t.v());
            cnt++;
            curr = t.h();
            if (nCols < t.f() + 1) {
                nCols = t.f() + 1;
            }
            if (curr != prev) {
                nRows++;
                rowStart.push_back(cnt - 1);
                landSeaMask.push_back(curr);
            }
            prev = curr;
        }
        nCols = triplets_.size();
        rowStart.push_back(nnz);

        INTERPOLATE_FESOM_OUT_STREAM << " - FesomIntermopationWeights: exit triplets2CSR"
                                     << (sizeof(T) == 4 ? "<single>" : "<double>") << std::endl;
    }

public:
    FesomInterpolationWeights(const std::vector<Tri>& triplets);


    bool initialized() const;


    void generateCacheFromTriplets(size_t NSide, orderingConvention_e orderingConvention, size_t level, size_t& nnz,
                                   size_t& nRows, size_t& nCols, size_t& nOutRows,
                                   std::vector<std::int32_t>& landSeaMask, std::vector<std::int32_t>& rowStart,
                                   std::vector<std::int32_t>& colIdx, std::vector<float>& values);


    void generateCacheFromTriplets(size_t NSide, orderingConvention_e orderingConvention, size_t level, size_t& nnz,
                                   size_t& nRows, size_t& nCols, size_t& nOutRows,
                                   std::vector<std::int32_t>& landSeaMask, std::vector<std::int32_t>& rowStart,
                                   std::vector<std::int32_t>& colIdx, std::vector<double>& values);

    void dumpTriplets() const;

    template <typename T>
    void dumpCache(const std::string& outputPath, const std::string& fesomName, const std::string& domain, size_t NSide,
                   orderingConvention_e orderingConvention, size_t level, size_t nnz, size_t nRows, size_t nCols,
                   size_t nOutRows, std::vector<std::int32_t>& landSeaMask, std::vector<std::int32_t>& rowStart,
                   std::vector<std::int32_t>& colIdx, std::vector<T>& values) const {

        INTERPOLATE_FESOM_OUT_STREAM << " - FesomIntermopationWeights: enter dumpCache"
                                     << (sizeof(T) == 4 ? "<single>" : "<double>") << std::endl;

        std::ostringstream os;
        os << outputPath << "/"
           << fesomCacheName(fesomName, domain, (sizeof(T) == 4 ? "single" : "double"), NSide, orderingConvention,
                             level)
           << ".atlas";

        eckit::codec::RecordWriter record;
        record.compression("none");
        record.set("version", static_cast<size_t>(0));
        record.set("nside", static_cast<size_t>(NSide));
        record.set("level", static_cast<size_t>(level));
        record.set("nnz", static_cast<size_t>(nnz));
        record.set("nRows", static_cast<size_t>(nRows));
        record.set("nCols", static_cast<size_t>(nCols));
        record.set("nOutRows", static_cast<size_t>(nOutRows));

        record.set("landSeaMask",
                   eckit::codec::ArrayReference(landSeaMask.data(), std::vector<size_t>{landSeaMask.size()}));
        record.set("rowPtr", eckit::codec::ArrayReference(rowStart.data(), std::vector<size_t>{rowStart.size()}));
        record.set("colIdx", eckit::codec::ArrayReference(colIdx.data(), std::vector<size_t>{colIdx.size()}));
        record.set("weights", eckit::codec::ArrayReference(values.data(), std::vector<size_t>{values.size()}));
        record.write(os.str());

        INTERPOLATE_FESOM_OUT_STREAM << " - FesomIntermopationWeights: exit dumpCache"
                                     << (sizeof(T) == 4 ? "<single>" : "<double>") << std::endl;

        return;
    }
};

}  // namespace multio::action::interpolate_fesom
