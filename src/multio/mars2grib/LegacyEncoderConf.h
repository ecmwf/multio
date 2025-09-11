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
    dm::Entry<TablesConfigurator> tables;
    dm::Entry<OriginConfigurator> origin;
    dm::Entry<DataTypeConfigurator> dataType;
    dm::Entry<ReferenceTimeConfigurator> referenceTime;

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
    dm::Entry<rules::PDTCat> pdtCat;

    // Param and Model are the only required configurators
    // All others are optional/product dependent
    dm::Entry<ParamConfigurator> param;
    dm::Entry<ModelConfigurator> model;

    // All optional in the sum of all products
    dm::Entry<PointInTimeConfigurator> pointInTime;
    dm::Entry<TimeRangeConfigurator> timeRange;
    dm::Entry<LevelConfigurator> level;
    dm::Entry<ProcessConfigurator> process;
    dm::Entry<RandomPatternsConfigurator> randomPatterns;
    dm::Entry<ChemConfigurator> chemical;
    dm::Entry<DirFreqConfigurator> dirFreq;
    dm::Entry<PeriodConfigurator> periodRange;
    dm::Entry<SatelliteConfigurator> satellite;

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

struct LegacySectionsConf {
    dm::Entry<std::string> type;

    dm::Entry<IndicatorSection> indicator;
    dm::Entry<IdentificationSection> identification;
    dm::Entry<LocalUseSection> localUse;
    dm::Entry<GridSection> grid;
    dm::Entry<ProductSection> product;
    dm::Entry<DataRepresSection> dataRepres;

    static constexpr std::string_view record_name_ = "encoder";
    static constexpr auto record_entries_ = std::make_tuple(
        dm::entryDef("type", &LegacySectionsConf::type).withDefault("grib2"),

        dm::nestedEntryDef(&LegacySectionsConf::indicator), dm::nestedEntryDef(&LegacySectionsConf::identification),
        dm::nestedEntryDef(&LegacySectionsConf::localUse), dm::nestedEntryDef(&LegacySectionsConf::grid),
        dm::nestedEntryDef(&LegacySectionsConf::product), dm::nestedEntryDef(&LegacySectionsConf::dataRepres)

    );
};


// Temporary solution - all in all the rules should directly map to the Grib2Structure
// inline dm::Grib2Structure toGrib2Structure(const LegacySectionsConf& conf) {
//     dm::Grib2Structure s;

//     s.tablesVersion.set(conf.identification.get().tables.get().tablesVersion);
//     s.localTablesVersion.set(conf.identification.get().tables.get().localTablesVersion);

//     s.localDefinitionNumber.set(conf.localUse.get().templateNumber.get());

//     // TODO(pgeier) Legacy - the MULTIOM code hacked in destine templates as 1036 and 1001
//     if (s.localDefinitionNumber.get() > 1000) {
//         s.localDefinitionNumber.set(s.localDefinitionNumber.get() - 1000);
//         s.destineLocalVersion.set(1);
//     }

//     // s.gridType.set(conf.grid.get().templateNumber.get());
//     s.productDefinitionTemplateNumber.set(conf.product.get().templateNumber.get());
//     s.dataRepresentationTemplateNumber.set(conf.dataRepres.get().templateNumber.get());

//     dm::applyRecordDefaults(s);
//     dm::validateRecord(s);
//     return s;
// };

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
struct Print<multio::mars2grib::LegacySectionsConf> : datamod::PrintRecord {};

}  // namespace util


template <>
struct datamod::ParseType<mars2grib::TablesConfigurator> : RecordMapper<mars2grib::TablesConfigurator> {};
template <>
struct datamod::DumpType<mars2grib::TablesConfigurator, eckit::LocalConfiguration> : RecordMapper<mars2grib::TablesConfigurator , eckit::LocalConfiguration> {};
template <>
struct datamod::ParseType<mars2grib::OriginConfigurator> : RecordMapper<mars2grib::OriginConfigurator> {};
template <>
struct datamod::DumpType<mars2grib::OriginConfigurator, eckit::LocalConfiguration> : RecordMapper<mars2grib::OriginConfigurator , eckit::LocalConfiguration> {};
template <>
struct datamod::ParseType<mars2grib::DataTypeConfigurator> : RecordMapper<mars2grib::DataTypeConfigurator> {};
template <>
struct datamod::DumpType<mars2grib::DataTypeConfigurator, eckit::LocalConfiguration> : RecordMapper<mars2grib::DataTypeConfigurator , eckit::LocalConfiguration> {};
template <>
struct datamod::ParseType<mars2grib::ReferenceTimeConfigurator> : RecordMapper<mars2grib::ReferenceTimeConfigurator> {};
template <>
struct datamod::DumpType<mars2grib::ReferenceTimeConfigurator, eckit::LocalConfiguration> : RecordMapper<mars2grib::ReferenceTimeConfigurator , eckit::LocalConfiguration> {};
template <>
struct datamod::ParseType<mars2grib::rules::PDTCat> : RecordMapper<mars2grib::rules::PDTCat> {};
template <>
struct datamod::DumpType<mars2grib::rules::PDTCat, eckit::LocalConfiguration> : RecordMapper<mars2grib::rules::PDTCat , eckit::LocalConfiguration> {};
template <>
struct datamod::ParseType<mars2grib::ParamConfigurator> : RecordMapper<mars2grib::ParamConfigurator> {};
template <>
struct datamod::DumpType<mars2grib::ParamConfigurator, eckit::LocalConfiguration> : RecordMapper<mars2grib::ParamConfigurator , eckit::LocalConfiguration> {};
template <>
struct datamod::ParseType<mars2grib::ModelConfigurator> : RecordMapper<mars2grib::ModelConfigurator> {};
template <>
struct datamod::DumpType<mars2grib::ModelConfigurator, eckit::LocalConfiguration> : RecordMapper<mars2grib::ModelConfigurator , eckit::LocalConfiguration> {};
template <>
struct datamod::ParseType<mars2grib::PointInTimeConfigurator> : RecordMapper<mars2grib::PointInTimeConfigurator> {};
template <>
struct datamod::DumpType<mars2grib::PointInTimeConfigurator, eckit::LocalConfiguration> : RecordMapper<mars2grib::PointInTimeConfigurator , eckit::LocalConfiguration> {};
template <>
struct datamod::ParseType<mars2grib::TimeRangeConfigurator> : RecordMapper<mars2grib::TimeRangeConfigurator> {};
template <>
struct datamod::DumpType<mars2grib::TimeRangeConfigurator, eckit::LocalConfiguration> : RecordMapper<mars2grib::TimeRangeConfigurator , eckit::LocalConfiguration> {};
template <>
struct datamod::ParseType<mars2grib::LevelConfigurator> : RecordMapper<mars2grib::LevelConfigurator> {};
template <>
struct datamod::DumpType<mars2grib::LevelConfigurator, eckit::LocalConfiguration> : RecordMapper<mars2grib::LevelConfigurator , eckit::LocalConfiguration> {};
template <>
struct datamod::ParseType<mars2grib::ProcessConfigurator> : RecordMapper<mars2grib::ProcessConfigurator> {};
template <>
struct datamod::DumpType<mars2grib::ProcessConfigurator, eckit::LocalConfiguration> : RecordMapper<mars2grib::ProcessConfigurator , eckit::LocalConfiguration> {};
template <>
struct datamod::ParseType<mars2grib::RandomPatternsConfigurator> : RecordMapper<mars2grib::RandomPatternsConfigurator> {
};
template <>
struct datamod::DumpType<mars2grib::RandomPatternsConfigurator, eckit::LocalConfiguration> : RecordMapper<mars2grib::RandomPatternsConfigurator, eckit::LocalConfiguration> {
};
template <>
struct datamod::ParseType<mars2grib::ChemConfigurator> : RecordMapper<mars2grib::ChemConfigurator> {};
template <>
struct datamod::DumpType<mars2grib::ChemConfigurator, eckit::LocalConfiguration> : RecordMapper<mars2grib::ChemConfigurator , eckit::LocalConfiguration> {};
template <>
struct datamod::ParseType<mars2grib::DirFreqConfigurator> : RecordMapper<mars2grib::DirFreqConfigurator> {};
template <>
struct datamod::DumpType<mars2grib::DirFreqConfigurator, eckit::LocalConfiguration> : RecordMapper<mars2grib::DirFreqConfigurator , eckit::LocalConfiguration> {};
template <>
struct datamod::ParseType<mars2grib::PeriodConfigurator> : RecordMapper<mars2grib::PeriodConfigurator> {};
template <>
struct datamod::DumpType<mars2grib::PeriodConfigurator, eckit::LocalConfiguration> : RecordMapper<mars2grib::PeriodConfigurator , eckit::LocalConfiguration> {};
template <>
struct datamod::ParseType<mars2grib::SatelliteConfigurator> : RecordMapper<mars2grib::SatelliteConfigurator> {};
template <>
struct datamod::DumpType<mars2grib::SatelliteConfigurator, eckit::LocalConfiguration> : RecordMapper<mars2grib::SatelliteConfigurator , eckit::LocalConfiguration> {};
template <>
struct datamod::ParseType<mars2grib::IndicatorSection> : RecordMapper<mars2grib::IndicatorSection> {};
template <>
struct datamod::DumpType<mars2grib::IndicatorSection, eckit::LocalConfiguration> : RecordMapper<mars2grib::IndicatorSection , eckit::LocalConfiguration> {};
template <>
struct datamod::ParseType<mars2grib::IdentificationSection> : RecordMapper<mars2grib::IdentificationSection> {};
template <>
struct datamod::DumpType<mars2grib::IdentificationSection, eckit::LocalConfiguration> : RecordMapper<mars2grib::IdentificationSection , eckit::LocalConfiguration> {};
template <>
struct datamod::ParseType<mars2grib::LocalUseSection> : RecordMapper<mars2grib::LocalUseSection> {};
template <>
struct datamod::DumpType<mars2grib::LocalUseSection, eckit::LocalConfiguration> : RecordMapper<mars2grib::LocalUseSection , eckit::LocalConfiguration> {};
template <>
struct datamod::ParseType<mars2grib::GridSection> : RecordMapper<mars2grib::GridSection> {};
template <>
struct datamod::DumpType<mars2grib::GridSection, eckit::LocalConfiguration> : RecordMapper<mars2grib::GridSection , eckit::LocalConfiguration> {};
template <>
struct datamod::ParseType<mars2grib::ProductSection> : RecordMapper<mars2grib::ProductSection> {};
template <>
struct datamod::DumpType<mars2grib::ProductSection, eckit::LocalConfiguration> : RecordMapper<mars2grib::ProductSection , eckit::LocalConfiguration> {};
template <>
struct datamod::ParseType<mars2grib::DataRepresSection> : RecordMapper<mars2grib::DataRepresSection> {};
template <>
struct datamod::DumpType<mars2grib::DataRepresSection, eckit::LocalConfiguration> : RecordMapper<mars2grib::DataRepresSection , eckit::LocalConfiguration> {};

}  // namespace multio

