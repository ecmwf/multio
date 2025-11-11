/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "TypeOfProcessedData.h"
#include "metkit/codes/api/CodesAPI.h"
#include "multio/datamod/core/DataModellingException.h"


namespace multio::datamod {

std::string DumpType<TypeOfProcessedData>::dump(TypeOfProcessedData v) {
    switch (v) {
        case TypeOfProcessedData::AnalysisProducts:
            return "an";
        case TypeOfProcessedData::ForecastProducts:
            return "fc";
        case TypeOfProcessedData::AnalysisAndForecastProducts:
            return "af";
        case TypeOfProcessedData::ControlForecastProducts:
            return "cf";
        case TypeOfProcessedData::PerturbedForecastProducts:
            return "pf";
        case TypeOfProcessedData::ControlAndPerturbedForecastProducts:
            return "cp";
        case TypeOfProcessedData::ProcessedSatelliteObservations:
            return "sa";
        case TypeOfProcessedData::ProcessedRadarObservations:
            return "ra";
        case TypeOfProcessedData::EventProbability:
            return "ep";
    }
    throw DataModellingException("DumpType<TypeOfProcessedData>::dump: Unexpected value for TypeOfProcessedData",
                                 Here());
}

int64_t DumpType<TypeOfProcessedData, metkit::codes::CodesHandle>::dump(TypeOfProcessedData v) {
    switch (v) {
        case TypeOfProcessedData::AnalysisProducts:
            return static_cast<int64_t>(TypeOfProcessedData::AnalysisProducts);
        case TypeOfProcessedData::ForecastProducts:
            return static_cast<int64_t>(TypeOfProcessedData::ForecastProducts);
        case TypeOfProcessedData::AnalysisAndForecastProducts:
            return static_cast<int64_t>(TypeOfProcessedData::AnalysisAndForecastProducts);
        case TypeOfProcessedData::ControlForecastProducts:
            return static_cast<int64_t>(TypeOfProcessedData::ControlForecastProducts);
        case TypeOfProcessedData::PerturbedForecastProducts:
            return static_cast<int64_t>(TypeOfProcessedData::PerturbedForecastProducts);
        case TypeOfProcessedData::ControlAndPerturbedForecastProducts:
            return static_cast<int64_t>(TypeOfProcessedData::ControlAndPerturbedForecastProducts);
        case TypeOfProcessedData::ProcessedSatelliteObservations:
            return static_cast<int64_t>(TypeOfProcessedData::ProcessedSatelliteObservations);
        case TypeOfProcessedData::ProcessedRadarObservations:
            return static_cast<int64_t>(TypeOfProcessedData::ProcessedRadarObservations);
        case TypeOfProcessedData::EventProbability:
            return static_cast<int64_t>(TypeOfProcessedData::EventProbability);
    }
    throw DataModellingException("DumpType<TypeOfProcessedData>::dump: Unexpected value for TypeOfProcessedData",
                                 Here());
}

TypeOfProcessedData ParseType<TypeOfProcessedData>::parse(const std::string& val) {
    if (val == "an") {
        return TypeOfProcessedData::AnalysisProducts;
    }
    if (val == "fc") {
        return TypeOfProcessedData::ForecastProducts;
    }
    if (val == "af") {
        return TypeOfProcessedData::AnalysisAndForecastProducts;
    }
    if (val == "cf") {
        return TypeOfProcessedData::ControlForecastProducts;
    }
    if (val == "pf") {
        return TypeOfProcessedData::PerturbedForecastProducts;
    }
    if (val == "cp") {
        return TypeOfProcessedData::ControlAndPerturbedForecastProducts;
    }
    if (val == "sa") {
        return TypeOfProcessedData::ProcessedSatelliteObservations;
    }
    if (val == "ra") {
        return TypeOfProcessedData::ProcessedRadarObservations;
    }
    if (val == "ep") {
        return TypeOfProcessedData::EventProbability;
    }

    throw DataModellingException(
        std::string("ParseType<TypeOfProcessedData>::parse Unknown value for TypeOfProcessedData: ") + val, Here());
}


TypeOfProcessedData ParseType<TypeOfProcessedData>::parse(int64_t i) {
    switch (i) {
        case static_cast<int64_t>(TypeOfProcessedData::AnalysisProducts):
            return TypeOfProcessedData::AnalysisProducts;
        case static_cast<int64_t>(TypeOfProcessedData::ForecastProducts):
            return TypeOfProcessedData::ForecastProducts;
        case static_cast<int64_t>(TypeOfProcessedData::AnalysisAndForecastProducts):
            return TypeOfProcessedData::AnalysisAndForecastProducts;
        case static_cast<int64_t>(TypeOfProcessedData::ControlForecastProducts):
            return TypeOfProcessedData::ControlForecastProducts;
        case static_cast<int64_t>(TypeOfProcessedData::PerturbedForecastProducts):
            return TypeOfProcessedData::PerturbedForecastProducts;
        case static_cast<int64_t>(TypeOfProcessedData::ControlAndPerturbedForecastProducts):
            return TypeOfProcessedData::ControlAndPerturbedForecastProducts;
        case static_cast<int64_t>(TypeOfProcessedData::ProcessedSatelliteObservations):
            return TypeOfProcessedData::ProcessedSatelliteObservations;
        case static_cast<int64_t>(TypeOfProcessedData::ProcessedRadarObservations):
            return TypeOfProcessedData::ProcessedRadarObservations;
        case static_cast<int64_t>(TypeOfProcessedData::EventProbability):
            return TypeOfProcessedData::EventProbability;
    };
    throw DataModellingException(
        std::string("ParseType<TypeOfProcessedData>::parse Unknown value for TypeOfProcessedData: ")
            + std::to_string(i),
        Here());
}

}  // namespace multio::datamod


namespace multio {

void util::Print<datamod::TypeOfProcessedData>::print(PrintStream& ps, const datamod::TypeOfProcessedData& t) {
    util::print(ps, datamod::TypeDumper<datamod::TypeOfProcessedData>::dump(t));
}

}  // namespace multio
