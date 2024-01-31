#include "FesomInterpolationWeights.h"

#include <algorithm>
#include <map>
#include <string>
#include <vector>
#include "InterpolateFesom_debug.h"

#define M_PI 3.14159265358979323846

namespace multio::action::interpolateFESOM {


Tri::Tri() : i_{0}, j_{0}, v_{0.0} {};
Tri::Tri(std::int32_t i, std::int32_t j, double v) : i_{i}, j_{j}, v_{v} {}


std::size_t Tri::h() const {
    return static_cast<std::size_t>(i_);
}
std::size_t Tri::f() const {
    return static_cast<std::size_t>(j_);
}
double Tri::v() const {
    return v_;
}


std::size_t Tri::idx(std::size_t scale) const {
    return static_cast<std::size_t>(i_) * scale + static_cast<std::size_t>(j_);
}

std::size_t Tri::reverse_idx(std::size_t scale) const {
    return static_cast<std::size_t>(j_) * scale + static_cast<std::size_t>(i_);
}


void Tri::print() const {
    std::cout << std::setw(15) << i_ << "   " << std::setw(15) << j_ << "   " << std::setw(35) << std::setprecision(25)
              << v_ << std::endl;
    return;
}

// -------------------------------------------------------------------------------------------------

std::string fesomCacheName(const std::string& fesomName, const std::string& domain, const std::string& precision,
                           size_t NSide, orderingConvention_e orderingConvention, double level) {
    INTERPOLATE_FESOM_OUT_STREAM << " - enter fesomCacheName" << std::endl;
    std::ostringstream os;
    std::string localDomain{domain};
    localDomain.erase(std::remove_if(localDomain.begin(), localDomain.end(), ::isspace), localDomain.end());
    std::transform(localDomain.begin(), localDomain.end(), localDomain.begin(),
                   [](unsigned char c) { return std::tolower(c); });
    INTERPOLATE_FESOM_OUT_STREAM << " - fesomCacheName: domain=" << domain << ", localDomain=" << localDomain
                                 << std::endl;
    os << "fesom_" << fesomName << "_" << localDomain << "_to_HEALPix_" << std::setw(6) << std::setfill('0') << NSide
       << "_" << precision << "_" << orderingConvention_enum2string(orderingConvention) << "_" << std::setw(8)
       << std::setfill('0') << static_cast<size_t>(std::fabs(level * 1000));
    INTERPOLATE_FESOM_OUT_STREAM << " - exit fesomCacheName" << std::endl;
    return os.str();
}

// -------------------------------------------------------------------------------------------------


orderingConvention_e orderingConvention_string2enum(const std::string& orderingConvention) {
    INTERPOLATE_FESOM_OUT_STREAM << " - enter orderingConvention_string2enum" << std::endl;
    if (orderingConvention != "ring" && orderingConvention != "nested") {
        std::ostringstream os;
        os << " - Unexpected value for \"orderingConvention\": "
           << "\"" << orderingConvention << "\"" << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    INTERPOLATE_FESOM_OUT_STREAM << " - exit orderingConvention_string2enum" << std::endl;
    return (orderingConvention == "ring" ? orderingConvention_e::RING : orderingConvention_e::NESTED);
}


std::string orderingConvention_enum2string(orderingConvention_e orderingConvention) {
    INTERPOLATE_FESOM_OUT_STREAM << " - enter orderingConvention_enum2string" << std::endl;
    if (orderingConvention == orderingConvention_e::UNKNOWN) {
        std::ostringstream os;
        os << " - Unexpected value for \"orderingConvention\": "
           << "\"UNKNOWN\"" << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    INTERPOLATE_FESOM_OUT_STREAM << " - exit orderingConvention_enum2string" << std::endl;
    return (orderingConvention == orderingConvention_e::RING ? "ring" : "nested");
}

// -------------------------------------------------------------------------------------------------

void FesomInterpolationWeights::clearTriplets() {

    INTERPOLATE_FESOM_OUT_STREAM << " - FesomIntermopationWeights: enter clearTriplets" << std::endl;

    if (!initialized_) {
        std::ostringstream os;
        os << "Object not initialized" << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }

    triplets_.clear();

    INTERPOLATE_FESOM_OUT_STREAM << " - FesomIntermopationWeights: exit clearTriplets" << std::endl;
}


void FesomInterpolationWeights::sortTriplets() {

    INTERPOLATE_FESOM_OUT_STREAM << " - FesomIntermopationWeights: enter sortTriplets" << std::endl;

    if (!initialized_) {
        std::ostringstream os;
        os << "Object not initialized" << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }

    std::sort(triplets_.begin(), triplets_.end(), [](Tri a, Tri b) { return a.idx(1000000000) < b.idx(1000000000); });

    INTERPOLATE_FESOM_OUT_STREAM << " - FesomIntermopationWeights: exit sortTriplets" << std::endl;
}


void FesomInterpolationWeights::reverse_sortTriplets() {

    INTERPOLATE_FESOM_OUT_STREAM << " - FesomIntermopationWeights: enter sortTriplets" << std::endl;

    if (!initialized_) {
        std::ostringstream os;
        os << "Object not initialized" << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }

    std::sort(triplets_.begin(), triplets_.end(), [](Tri a, Tri b) { return a.reverse_idx(1000000000) < b.reverse_idx(1000000000); });

    INTERPOLATE_FESOM_OUT_STREAM << " - FesomIntermopationWeights: exit sortTriplets" << std::endl;
}


void FesomInterpolationWeights::dumpTriplets() const {

    INTERPOLATE_FESOM_OUT_STREAM << " - FesomIntermopationWeights: enter dumpTriplets" << std::endl;

    for (const auto& t : triplets_) {
        t.print();
    }

    INTERPOLATE_FESOM_OUT_STREAM << " - FesomIntermopationWeights: exit dumpTriplets" << std::endl;
}


FesomInterpolationWeights::FesomInterpolationWeights(const std::vector<Tri>& triplets) : initialized_{false} {

    INTERPOLATE_FESOM_OUT_STREAM << " - FesomIntermopationWeights: enter FesomInterpolationWeights (from triplets)"
                                 << std::endl;

    initialized_ = true;
    clearTriplets();
    INTERPOLATE_FESOM_OUT_STREAM << " - FesomIntermopationWeights: copy triplets: " << size(triplets) << std::endl;
    for (const auto& t : triplets) {
        triplets_.push_back(t);
    }
    sortTriplets();

    INTERPOLATE_FESOM_OUT_STREAM << " - FesomIntermopationWeights: exit FesomInterpolationWeights (from triplets)"
                                 << std::endl;
}


bool FesomInterpolationWeights::initialized() const {
    return initialized_;
}

void FesomInterpolationWeights::generateCacheFromTriplets(size_t NSide, orderingConvention_e orderingConvention,
                                                          size_t level, size_t& nnz, size_t& nRows, size_t& nCols,
                                                          size_t& nOutRows, std::vector<std::int32_t>& landSeaMask,
                                                          std::vector<std::int32_t>& rowStart,
                                                          std::vector<std::int32_t>& colIdx,
                                                          std::vector<float>& values) {

    INTERPOLATE_FESOM_OUT_STREAM << " - FesomIntermopationWeights: enter generateCache<float>" << std::endl;
    if (!initialized_) {
        std::ostringstream os;
        os << "Object not initialized" << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    triplets2CSR<float>(NSide, level, nnz, nRows, nCols, nOutRows, landSeaMask, rowStart, colIdx, values);

    INTERPOLATE_FESOM_OUT_STREAM << " - FesomIntermopationWeights: exit generateCache<float>" << std::endl;
}


void FesomInterpolationWeights::generateCacheFromTriplets(size_t NSide, orderingConvention_e orderingConvention,
                                                          size_t level, size_t& nnz, size_t& nRows, size_t& nCols,
                                                          size_t& nOutRows, std::vector<std::int32_t>& landSeaMask,
                                                          std::vector<std::int32_t>& rowStart,
                                                          std::vector<std::int32_t>& colIdx,
                                                          std::vector<double>& values) {

    INTERPOLATE_FESOM_OUT_STREAM << " - FesomIntermopationWeights: enter generateCache<double>" << std::endl;

    if (!initialized_) {
        std::ostringstream os;
        os << "Object not initialized" << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    triplets2CSR<double>(NSide, level, nnz, nRows, nCols, nOutRows, landSeaMask, rowStart, colIdx, values);

    INTERPOLATE_FESOM_OUT_STREAM << " - FesomIntermopationWeights: exit generateCache<double>" << std::endl;
}

}  // namespace multio::action::interpolateFESOM
