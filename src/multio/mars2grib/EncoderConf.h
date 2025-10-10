/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#pragma once

#include "multio/datamod/GribKeys.h"
#include "multio/datamod/core/Compare.h"
#include "multio/datamod/core/EntryDef.h"
#include "multio/datamod/core/NestedRecord.h"
#include "multio/datamod/core/Record.h"
#include "multio/mars2grib/Mars2GribException.h"
#include "multio/mars2grib/generated/InferPDT.h"
#include "multio/mars2grib/sections/Level.h"
#include "multio/mars2grib/sections/SectionTypes.h"

namespace multio {

/// Detailed descriptions of the encoder configuration.
/// First attempt is to replicate existing structure and to migrate the rule
/// search mechanism while keeping the fortran encoders.
/// Often the structures of the configs have a subconfiguration where to only
/// available key-value is "type": "default" These have to be refactored to
/// something less but more expressive
///
/// TODO: After the migration this should become a proper struct with optionals
///       Encoding/Parsing will not be needed then.
///       It was a quick way to represent the current encoder configuration and
///       to set keys on it

namespace dm = multio::datamod;

namespace mars2grib {

//----------------------------------------------------------------------------------------------------------------------
// Section0
//----------------------------------------------------------------------------------------------------------------------

struct IndicatorSection {
    dm::Entry<std::int64_t> templateNumber;

    static constexpr std::string_view record_name_ = "indicator-section";
    static constexpr auto record_entries_
        = std::make_tuple(dm::entryDef("template-number", &IndicatorSection::templateNumber).withDefault(0));
};

//----------------------------------------------------------------------------------------------------------------------
// Section1
//----------------------------------------------------------------------------------------------------------------------

struct OriginConfigurator {
    dm::Entry<std::string> type;
    dm::Entry<std::int64_t> subCentre;

    static constexpr std::string_view record_name_ = "origin-configurator";
    static constexpr auto record_entries_
        = std::make_tuple(dm::entryDef("type", &OriginConfigurator::type).withDefault("default"),
                          dm::entryDef("sub-centre", &OriginConfigurator::subCentre).withDefault(0));
};

struct DataTypeConfigurator {
    dm::Entry<std::string> type;

    static constexpr std::string_view record_name_ = "data-type-configurator";
    static constexpr auto record_entries_
        = std::make_tuple(dm::entryDef("type", &DataTypeConfigurator::type).withDefault("default"));
};

struct ReferenceTimeConfigurator {
    dm::Entry<std::string> type;

    static constexpr std::string_view record_name_ = "reference-time-configurator";
    static constexpr auto record_entries_
        = std::make_tuple(dm::entryDef("type", &ReferenceTimeConfigurator::type).withDefault("default"));
};

struct TablesConfigurator {
    dm::Entry<std::string> type;
    dm::Entry<std::int64_t> tablesVersion;
    dm::Entry<std::int64_t> localTablesVersion;

    static constexpr std::string_view record_name_ = "tables-configurator";
    static constexpr auto record_entries_
        = std::make_tuple(dm::entryDef("type", &TablesConfigurator::type).withDefault("default"),
                          dm::entryDef("tables-version", &TablesConfigurator::tablesVersion).tagOptional(),
                          dm::entryDef("local-tables-version", &TablesConfigurator::localTablesVersion).withDefault(0));
};

// Whole identification section
struct IdentificationSection {
    dm::Entry<std::int64_t> templateNumber;
    dm::NestedEntry_t<TablesConfigurator> tables;
    dm::NestedEntry_t<OriginConfigurator> origin;
    dm::NestedEntry_t<DataTypeConfigurator> dataType;
    dm::NestedEntry_t<ReferenceTimeConfigurator> referenceTime;

    static constexpr std::string_view record_name_ = "identification-section";
    static constexpr auto record_entries_ = std::make_tuple(
        dm::entryDef("template-number", &IdentificationSection::templateNumber).withDefault(0),
        dm::nestedEntryDef(&IdentificationSection::origin), dm::nestedEntryDef(&IdentificationSection::dataType),
        dm::nestedEntryDef(&IdentificationSection::referenceTime), dm::nestedEntryDef(&IdentificationSection::tables));
};

//----------------------------------------------------------------------------------------------------------------------
// Section2 Local use
//----------------------------------------------------------------------------------------------------------------------

struct LocalUseSection {
    dm::Entry<std::int64_t> templateNumber;

    static constexpr std::string_view record_name_ = "local-use-section";
    static constexpr auto record_entries_
        = std::make_tuple(dm::entryDef("template-number", &LocalUseSection::templateNumber).withDefault(0));
};

//----------------------------------------------------------------------------------------------------------------------
// Section3 Grid
//----------------------------------------------------------------------------------------------------------------------

struct GridSection {
    dm::Entry<std::int64_t> templateNumber;

    static constexpr std::string_view record_name_ = "grid-definition-section";
    static constexpr auto record_entries_
        = std::make_tuple(dm::entryDef("template-number", &GridSection::templateNumber).withDefault(0));
};

//----------------------------------------------------------------------------------------------------------------------
// Section4 Product definition
//----------------------------------------------------------------------------------------------------------------------

struct ParamConfigurator {
    dm::Entry<std::string> type;
    dm::Entry<std::string> datasetForLocal;

    static constexpr std::string_view record_name_ = "param-configurator";
    static constexpr auto record_entries_
        = std::make_tuple(dm::entryDef("type", &ParamConfigurator::type).withDefault("paramId"),
                          dm::entryDef("dataset-for-local", &ParamConfigurator::datasetForLocal).tagOptional());
};

struct PointInTimeConfigurator {
    dm::Entry<std::string> type;

    static constexpr std::string_view record_name_ = "point-in-time-configurator";
    static constexpr auto record_entries_
        = std::make_tuple(dm::entryDef("type", &PointInTimeConfigurator::type).withDefault("default"));
};

struct TimeRangeConfigurator {
    dm::Entry<sections::TimeRangeType> type;
    dm::Entry<dm::TypeOfStatisticalProcessing> typeOfStatisticalProcessing;
    dm::Entry<std::string> overallLengthOfTimeRange;

    static constexpr std::string_view record_name_ = "time-statistics-configurator";
    static constexpr auto record_entries_ = std::make_tuple(
        dm::entryDef("type", &TimeRangeConfigurator::type),
        dm::entryDef("type-of-statistical-processing", &TimeRangeConfigurator::typeOfStatisticalProcessing),
        dm::entryDef("overall-length-of-timerange", &TimeRangeConfigurator::overallLengthOfTimeRange).tagOptional());
};

struct ProcessConfigurator {
    dm::Entry<std::string> type;

    static constexpr std::string_view record_name_ = "ensemble-configurator";
    static constexpr auto record_entries_
        = std::make_tuple(dm::entryDef("type", &ProcessConfigurator::type).withDefault("default"));
};

struct ModelConfigurator {
    dm::Entry<std::string> type;

    static constexpr std::string_view record_name_ = "model-configurator";
    static constexpr auto record_entries_
        = std::make_tuple(dm::entryDef("type", &ModelConfigurator::type).withDefault("default"));
};

struct RandomPatternsConfigurator {
    dm::Entry<std::string> type;

    static constexpr std::string_view record_name_ = "random-patterns-configurator";
    static constexpr auto record_entries_
        = std::make_tuple(dm::entryDef("type", &RandomPatternsConfigurator::type).withDefault("default"));
};

struct ChemConfigurator {
    dm::Entry<std::string> type;

    static constexpr std::string_view record_name_ = "chemistry-configurator";
    static constexpr auto record_entries_
        = std::make_tuple(dm::entryDef("type", &ChemConfigurator::type).withDefault("chemical"));
};

struct DirFreqConfigurator {
    dm::Entry<std::string> type;

    static constexpr std::string_view record_name_ = "directions-frequencies-configurator";
    static constexpr auto record_entries_
        = std::make_tuple(dm::entryDef("type", &DirFreqConfigurator::type).withDefault("default")

        );
};

struct SatelliteConfigurator {
    dm::Entry<std::string> type;

    static constexpr std::string_view record_name_ = "satellite-configurator";
    static constexpr auto record_entries_
        = std::make_tuple(dm::entryDef("type", &SatelliteConfigurator::type).withDefault("default")

        );
};

struct PeriodConfigurator {
    dm::Entry<std::string> type;

    static constexpr std::string_view record_name_ = "period-configurator";
    static constexpr auto record_entries_
        = std::make_tuple(dm::entryDef("type", &PeriodConfigurator::type).withDefault("default"));
};

using multio::mars2grib::sections::LevelConfigurator;

struct ProductSection {
    dm::Entry<std::int64_t> templateNumber;
    // Optional as it is used to generate templateNumber
    dm::NestedEntry_t<rules::PDTCat> pdtCat;

    // Param and Model are the only required configurators
    // All others are optional/product dependent
    dm::NestedEntry_t<ParamConfigurator> param;
    dm::NestedEntry_t<ModelConfigurator> model;

    // All optional in the sum of all products
    dm::NestedEntry_t<PointInTimeConfigurator> pointInTime;
    dm::NestedEntry_t<TimeRangeConfigurator> timeRange;
    dm::NestedEntry_t<LevelConfigurator> level;
    dm::NestedEntry_t<ProcessConfigurator> process;
    dm::NestedEntry_t<RandomPatternsConfigurator> randomPatterns;
    dm::NestedEntry_t<ChemConfigurator> chemical;
    dm::NestedEntry_t<DirFreqConfigurator> dirFreq;
    dm::NestedEntry_t<PeriodConfigurator> periodRange;
    dm::NestedEntry_t<SatelliteConfigurator> satellite;

    static constexpr std::string_view record_name_ = "product-definition-section";
    static constexpr auto record_entries_ = std::make_tuple(
        dm::entryDef("template-number", &ProductSection::templateNumber),

        dm::nestedOptEntryDef(&ProductSection::pdtCat),

        dm::nestedEntryDef(&ProductSection::param), dm::nestedEntryDef(&ProductSection::model),

        dm::nestedOptEntryDef(&ProductSection::pointInTime), dm::nestedOptEntryDef(&ProductSection::timeRange),
        dm::nestedOptEntryDef(&ProductSection::level), dm::nestedOptEntryDef(&ProductSection::process),
        dm::nestedOptEntryDef(&ProductSection::level), dm::nestedOptEntryDef(&ProductSection::randomPatterns),
        dm::nestedOptEntryDef(&ProductSection::chemical), dm::nestedOptEntryDef(&ProductSection::dirFreq),
        dm::nestedOptEntryDef(&ProductSection::periodRange), dm::nestedOptEntryDef(&ProductSection::satellite));

    static void applyDefaults(mars2grib::ProductSection& product) {
        using namespace mars2grib;
        using namespace mars2grib::rules;

        // Checking PDT
        {
            if (product.pdtCat.isSet()) {
                auto pdtNum = InferPdt<>{}.inferProductDefinitionTemplateNumber(product.pdtCat.get());

                if (product.templateNumber.isSet() && product.templateNumber.get() != pdtNum) {
                    std::ostringstream oss;
                    oss << "ProductSection configuration has a template number and PDT "
                           "categories specified, but the "
                           "generated PDT "
                        << pdtNum << " is different from the passed " << product.templateNumber.get();
                    throw Mars2GribException(oss.str(), Here());
                }
                product.templateNumber.set(pdtNum);
            }
        }
    }

    static void validate(mars2grib::ProductSection& product) {
        using namespace mars2grib;
        using namespace mars2grib::rules;

        // Checking PDT
        {
            if (!product.templateNumber.isSet() && !product.pdtCat.isSet()) {
                std::ostringstream oss;
                oss << "ProductSection configuration has no template number and no PDT "
                       "categories  specified.";
                throw Mars2GribException(oss.str(), Here());
            }
        }

        // Checking Time
        {
            if (product.timeRange.isSet() && product.pointInTime.isSet()) {
                std::ostringstream oss;
                oss << "ProductSection configuration has a PointInTime and a "
                       "TimeStatistics section."
                    << std::endl;
                throw Mars2GribException(oss.str(), Here());
            }
            if (!product.timeRange.isSet() && !product.pointInTime.isSet()) {
                std::ostringstream oss;
                oss << "ProductSection configuration has no time definition." << std::endl;
                throw Mars2GribException(oss.str(), Here());
            }
        }
    }
};


//----------------------------------------------------------------------------------------------------------------------
// Section5 Data representation
//----------------------------------------------------------------------------------------------------------------------

struct DataRepresSection {
    dm::Entry<std::int64_t> templateNumber;

    static constexpr std::string_view record_name_ = "data-representation-section";
    static constexpr auto record_entries_
        = std::make_tuple(dm::entryDef("template-number", &DataRepresSection::templateNumber).withDefault(0)

        );
};

//----------------------------------------------------------------------------------------------------------------------
// All sections
//----------------------------------------------------------------------------------------------------------------------

struct SectionsConf {
    dm::Entry<std::string> type;

    dm::NestedEntry_t<IndicatorSection> indicator;
    dm::NestedEntry_t<IdentificationSection> identification;
    dm::NestedEntry_t<LocalUseSection> localUse;
    dm::NestedEntry_t<GridSection> grid;
    dm::NestedEntry_t<ProductSection> product;
    dm::NestedEntry_t<DataRepresSection> dataRepres;

    static constexpr std::string_view record_name_ = "encoder";
    static constexpr auto record_entries_ = std::make_tuple(
        dm::entryDef("type", &SectionsConf::type).withDefault("grib2"),

        dm::nestedEntryDef(&SectionsConf::indicator), dm::nestedEntryDef(&SectionsConf::identification),
        dm::nestedEntryDef(&SectionsConf::localUse), dm::nestedEntryDef(&SectionsConf::grid),
        dm::nestedEntryDef(&SectionsConf::product), dm::nestedEntryDef(&SectionsConf::dataRepres)

    );
};
}  // namespace mars2grib

// //---------------------------------------------------------------------------------------------------------------------

// }  // namespace mars2grib

namespace util {

template <>
struct Print<multio::mars2grib::IndicatorSection> : datamod::PrintRecord {};
template <>
struct Print<multio::mars2grib::OriginConfigurator> : datamod::PrintRecord {};
template <>
struct Print<multio::mars2grib::DataTypeConfigurator> : datamod::PrintRecord {};
template <>
struct Print<multio::mars2grib::ReferenceTimeConfigurator> : datamod::PrintRecord {};
template <>
struct Print<multio::mars2grib::TablesConfigurator> : datamod::PrintRecord {};
template <>
struct Print<multio::mars2grib::IdentificationSection> : datamod::PrintRecord {};
template <>
struct Print<multio::mars2grib::LocalUseSection> : datamod::PrintRecord {};
template <>
struct Print<multio::mars2grib::GridSection> : datamod::PrintRecord {};
template <>
struct Print<multio::mars2grib::ParamConfigurator> : datamod::PrintRecord {};
template <>
struct Print<multio::mars2grib::PointInTimeConfigurator> : datamod::PrintRecord {};
template <>
struct Print<multio::mars2grib::TimeRangeConfigurator> : datamod::PrintRecord {};
template <>
struct Print<multio::mars2grib::ProcessConfigurator> : datamod::PrintRecord {};
template <>
struct Print<multio::mars2grib::ModelConfigurator> : datamod::PrintRecord {};
template <>
struct Print<multio::mars2grib::RandomPatternsConfigurator> : datamod::PrintRecord {};
template <>
struct Print<multio::mars2grib::SatelliteConfigurator> : datamod::PrintRecord {};
template <>
struct Print<multio::mars2grib::ChemConfigurator> : datamod::PrintRecord {};
template <>
struct Print<multio::mars2grib::DirFreqConfigurator> : datamod::PrintRecord {};
template <>
struct Print<multio::mars2grib::PeriodConfigurator> : datamod::PrintRecord {};
template <>
struct Print<multio::mars2grib::ProductSection> : datamod::PrintRecord {};
template <>
struct Print<multio::mars2grib::DataRepresSection> : datamod::PrintRecord {};
template <>
struct Print<multio::mars2grib::SectionsConf> : datamod::PrintRecord {};

}  // namespace util

}  // namespace multio
