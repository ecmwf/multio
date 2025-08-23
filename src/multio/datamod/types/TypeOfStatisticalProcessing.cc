/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "TypeOfStatisticalProcessing.h"
#include "multio/datamod/core/DataModellingException.h"


namespace multio::datamod {

std::string DumpType<TypeOfStatisticalProcessing>::dump(TypeOfStatisticalProcessing v) {
    switch (v) {
        case TypeOfStatisticalProcessing::Average:
            return "average";
        case TypeOfStatisticalProcessing::Accumulation:
            return "accumul";
        case TypeOfStatisticalProcessing::Maximum:
            return "max";
        case TypeOfStatisticalProcessing::Minimum:
            return "min";
        case TypeOfStatisticalProcessing::Difference:
            return "difference";
        case TypeOfStatisticalProcessing::RootMeanSquare:
            return "root-mean-square";
        case TypeOfStatisticalProcessing::StandardDeviation:
            return "stddev";  // Currently used in MULTIOM...
        case TypeOfStatisticalProcessing::Covariance:
            return "covariance";
        case TypeOfStatisticalProcessing::InverseDifference:
            return "inverse-difference";  // TODO rename? stepType is sdiff
        case TypeOfStatisticalProcessing::Ratio:
            return "ratio";
        case TypeOfStatisticalProcessing::StandardizedAnomaly:
            return "standardized-anomaly";
        case TypeOfStatisticalProcessing::Summation:
            return "summation";
        case TypeOfStatisticalProcessing::ReturnPeriod:
            return "return-period";
        case TypeOfStatisticalProcessing::Median:
            return "median";
        case TypeOfStatisticalProcessing::Severity:
            return "severity";
        case TypeOfStatisticalProcessing::Mode:
            return "mode";
        case TypeOfStatisticalProcessing::IndexProcessing:
            return "index-processing";
        default:
            throw DataModellingException(
                "DumpType<TypeOfStatisticalProcessing>::dump: Unexpected value for TypeOfStatisticalProcessing",
                Here());
    }
}

TypeOfStatisticalProcessing ParseType<TypeOfStatisticalProcessing>::parse(const std::string& val) {
    // May use a vector
    static const std::vector<std::pair<std::string, TypeOfStatisticalProcessing>> typesOfStat{
        {"average", TypeOfStatisticalProcessing::Average},
        {"accumul", TypeOfStatisticalProcessing::Accumulation},
        {"max", TypeOfStatisticalProcessing::Maximum},
        {"min", TypeOfStatisticalProcessing::Minimum},
        {"difference", TypeOfStatisticalProcessing::Difference},
        {"root-mean-square", TypeOfStatisticalProcessing::RootMeanSquare},
        {"stddev", TypeOfStatisticalProcessing::StandardDeviation},
        {"covariance", TypeOfStatisticalProcessing::Covariance},
        {"inverse-difference", TypeOfStatisticalProcessing::InverseDifference},  // TODO rename? stepType is sdiff
        {"ratio", TypeOfStatisticalProcessing::Ratio},
        {"standardized-anomaly", TypeOfStatisticalProcessing::StandardizedAnomaly},
        {"summation", TypeOfStatisticalProcessing::Summation},
        {"return-period", TypeOfStatisticalProcessing::ReturnPeriod},
        {"median", TypeOfStatisticalProcessing::Median},
        {"severity", TypeOfStatisticalProcessing::Severity},
        {"mode", TypeOfStatisticalProcessing::Mode},
        {"index-processing", TypeOfStatisticalProcessing::IndexProcessing},
    };

    if (auto tos
        = std::find_if(typesOfStat.begin(), typesOfStat.end(), [&](const auto& pair) { return val == pair.first; });
        tos != typesOfStat.end()) {
        return tos->second;
    }
    throw DataModellingException(
        std::string("ParseType<TypeOfStatisticalProcessing>::parse Unknown value for TypeOfStatisticalProcessing: ")
            + val,
        Here());
}


TypeOfStatisticalProcessing ParseType<TypeOfStatisticalProcessing>::parse(std::int64_t i) {
    switch (i) {
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::Average):
            return TypeOfStatisticalProcessing::Average;
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::Accumulation):
            return TypeOfStatisticalProcessing::Accumulation;
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::Maximum):
            return TypeOfStatisticalProcessing::Maximum;
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::Minimum):
            return TypeOfStatisticalProcessing::Minimum;
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::Difference):
            return TypeOfStatisticalProcessing::Difference;
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::RootMeanSquare):
            return TypeOfStatisticalProcessing::RootMeanSquare;
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::StandardDeviation):
            return TypeOfStatisticalProcessing::StandardDeviation;
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::Covariance):
            return TypeOfStatisticalProcessing::Covariance;
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::InverseDifference):
            return TypeOfStatisticalProcessing::InverseDifference;
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::Ratio):
            return TypeOfStatisticalProcessing::Ratio;
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::StandardizedAnomaly):
            return TypeOfStatisticalProcessing::StandardizedAnomaly;
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::Summation):
            return TypeOfStatisticalProcessing::Summation;
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::ReturnPeriod):
            return TypeOfStatisticalProcessing::ReturnPeriod;
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::Median):
            return TypeOfStatisticalProcessing::Median;
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::Severity):
            return TypeOfStatisticalProcessing::Severity;
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::Mode):
            return TypeOfStatisticalProcessing::Mode;
        case static_cast<std::int64_t>(TypeOfStatisticalProcessing::IndexProcessing):
            return TypeOfStatisticalProcessing::IndexProcessing;
        default:
            throw DataModellingException(
                std::string(
                    "ParseType<TypeOfStatisticalProcessing>::parse Unknown value for TypeOfStatisticalProcessing: ")
                    + std::to_string(i),
                Here());
    };
}

}  // namespace multio::datamod


namespace multio {

void util::Print<datamod::TypeOfStatisticalProcessing>::print(PrintStream& ps,
                                                              const datamod::TypeOfStatisticalProcessing& t) {
    util::print(ps, datamod::TypeDumper<datamod::TypeOfStatisticalProcessing>::dump(t));
}

}  // namespace multio

