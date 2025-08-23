/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#pragma once

#include "eckit/config/LocalConfiguration.h"
#include "multio/datamod/GribKeys.h"
#include "multio/datamod/core/Compare.h"
#include "multio/datamod/core/Entry.h"
#include "multio/datamod/core/EntryDef.h"
#include "multio/datamod/core/NestedRecord.h"
#include "multio/datamod/core/Record.h"
#include "multio/mars2grib/Mars2GribException.h"
#include "multio/mars2grib/generated/InferPDT.h"
#include "multio/mars2grib/sections/Level.h"
#include "multio/mars2grib/sections/SectionTypes.h"

#include <memory>


namespace multio {

//-----------------------------------------------------------------------------
// Detailed descriptions of the encoder configuration.
// First attempt is to replicate existing structure and to migrate the rule
// search mechanism while keeping the fortran encoders.
// Often the structures of the configs have a subconfiguration where to only available key-value is "type": "default"
// These have to be refactored to something less but more expressive
//
// TODO: After the migration this should become a proper struct with optionals
//       Encoding/Parsing will not be needed then.
//       It was a quick way to represent the current encoder configuration and
//       to set keys on it
//-----------------------------------------------------------------------------

namespace dm = multio::datamod;

//-----------------------------------------------------------------------------
// Section0
//-----------------------------------------------------------------------------


namespace mars2grib {

constexpr auto IndicatorTemplateNumber =           //
    dm::EntryDef<std::int64_t>{"template-number"}  //
        .withDefault(0)
        .withAccessor([](auto&& v) { return &v.templateNumber; });

struct IndicatorSection {
    dm::EntryType_t<decltype(IndicatorTemplateNumber)> templateNumber;

    static constexpr std::string_view record_name_ = "indicator-section";
    static constexpr auto record_entries_ = std::make_tuple(IndicatorTemplateNumber);
};

}  // namespace mars2grib

//-----------------------------------------------------------------------------
// Section1
//-----------------------------------------------------------------------------

// Origin configurator
namespace mars2grib {

constexpr auto OriginType =            //
    dm::EntryDef<std::string>{"type"}  //
        .withDefault("default")
        .withAccessor([](auto&& v) { return &v.type; });

constexpr auto OriginSubCentre =              //
    dm::EntryDef<std::int64_t>{"sub-centre"}  //
        .withDefault(0)
        .withAccessor([](auto&& v) { return &v.subCentre; });

struct OriginConfigurator {
    dm::EntryType_t<decltype(OriginType)> type;
    dm::EntryType_t<decltype(OriginSubCentre)> subCentre;

    static constexpr std::string_view record_name_ = "origin-configurator";
    static constexpr auto record_entries_ = std::make_tuple(OriginType, OriginSubCentre);
};

}  // namespace mars2grib


namespace mars2grib {
constexpr auto DataType =              //
    dm::EntryDef<std::string>{"type"}  //
        .withDefault("default")
        .withAccessor([](auto&& v) { return &v.type; });

struct DataTypeConfigurator {
    dm::EntryType_t<decltype(DataType)> type;

    static constexpr std::string_view record_name_ = "data-type-configurator";
    static constexpr auto record_entries_ = std::make_tuple(DataType);
};
}  // namespace mars2grib


// ReferenceTime configurator
namespace mars2grib {
constexpr auto ReferenceTimeType =     //
    dm::EntryDef<std::string>{"type"}  //
        .withDefault("default")
        .withAccessor([](auto&& v) { return &v.type; });

struct ReferenceTimeConfigurator {
    dm::EntryType_t<decltype(ReferenceTimeType)> type;

    static constexpr std::string_view record_name_ = "reference-time-configurator";
    static constexpr auto record_entries_ = std::make_tuple(ReferenceTimeType);
};
}  // namespace mars2grib


// Tables configurator
namespace mars2grib {
constexpr auto TablesType =            //
    dm::EntryDef<std::string>{"type"}  //
        .withDefault("default")
        .withAccessor([](auto&& v) { return &v.type; });
constexpr auto TablesVersion =                    //
    dm::EntryDef<std::int64_t>{"tables-version"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.tablesVersion; });
constexpr auto LocalTablesVersion =                     //
    dm::EntryDef<std::int64_t>{"local-tables-version"}  //
        .withDefault(0)
        .withAccessor([](auto&& v) { return &v.localTablesVersion; });

struct TablesConfigurator {
    dm::EntryType_t<decltype(TablesType)> type;
    dm::EntryType_t<decltype(TablesVersion)> tablesVersion;
    dm::EntryType_t<decltype(LocalTablesVersion)> localTablesVersion;

    static constexpr std::string_view record_name_ = "tables-configurator";
    static constexpr auto record_entries_ = std::make_tuple(TablesType, TablesVersion, LocalTablesVersion);
};
}  // namespace mars2grib


// Whole identification section
namespace mars2grib {

constexpr auto IdentificationTemplateNumber =      //
    dm::EntryDef<std::int64_t>{"template-number"}  //
        .withDefault(0)
        .withAccessor([](auto&& v) { return &v.templateNumber; });


constexpr auto NestedTablesConfigurator =   //
    dm::nestedRecord<TablesConfigurator>()  //
        .withAccessor([](auto&& v) { return &v.tables; });

constexpr auto NestedOriginConfigurator =   //
    dm::nestedRecord<OriginConfigurator>()  //
        .withAccessor([](auto&& v) { return &v.origin; });

constexpr auto NestedDataTypeConfigurator =   //
    dm::nestedRecord<DataTypeConfigurator>()  //
        .withAccessor([](auto&& v) { return &v.dataType; });

constexpr auto NestedReferenceTimeConfigurator =   //
    dm::nestedRecord<ReferenceTimeConfigurator>()  //
        .withAccessor([](auto&& v) { return &v.referenceTime; });


struct IdentificationSection {
    dm::EntryType_t<decltype(IdentificationTemplateNumber)> templateNumber;
    dm::EntryType_t<decltype(NestedOriginConfigurator)> origin;
    dm::EntryType_t<decltype(NestedDataTypeConfigurator)> dataType;
    dm::EntryType_t<decltype(NestedReferenceTimeConfigurator)> referenceTime;
    dm::EntryType_t<decltype(NestedTablesConfigurator)> tables;

    static constexpr std::string_view record_name_ = "identification-section";
    static constexpr auto record_entries_
        = std::make_tuple(IdentificationTemplateNumber, NestedOriginConfigurator, NestedDataTypeConfigurator,
                          NestedReferenceTimeConfigurator, NestedTablesConfigurator);
};
}  // namespace mars2grib

//-----------------------------------------------------------------------------
// Section2 Local use
//-----------------------------------------------------------------------------

namespace mars2grib {

constexpr auto LocalUseTemplateNumber =            //
    dm::EntryDef<std::int64_t>{"template-number"}  //
        .withDefault(0)
        .withAccessor([](auto&& v) { return &v.templateNumber; });

struct LocalUseSection {
    dm::EntryType_t<decltype(LocalUseTemplateNumber)> templateNumber;

    static constexpr std::string_view record_name_ = "local-use-section";
    static constexpr auto record_entries_ = std::make_tuple(LocalUseTemplateNumber);
};

}  // namespace mars2grib


//-----------------------------------------------------------------------------
// Section3 Grid
//-----------------------------------------------------------------------------

namespace mars2grib {

constexpr auto GridTemplateNumber =                //
    dm::EntryDef<std::int64_t>{"template-number"}  //
        .withDefault(0)
        .withAccessor([](auto&& v) { return &v.templateNumber; });

struct GridSection {
    dm::EntryType_t<decltype(GridTemplateNumber)> templateNumber;

    static constexpr std::string_view record_name_ = "grid-definition-section";
    static constexpr auto record_entries_ = std::make_tuple(GridTemplateNumber);
};

}  // namespace mars2grib


//-----------------------------------------------------------------------------
// Section4 Product definition
//-----------------------------------------------------------------------------

// Param
namespace mars2grib {
constexpr auto ParamType =             //
    dm::EntryDef<std::string>{"type"}  //
        .withDefault("paramId")
        .withAccessor([](auto&& v) { return &v.type; });
constexpr auto DatasetForLocal =                    //
    dm::EntryDef<std::string>{"dataset-for-local"}  //
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.datasetForLocal; });

struct ParamConfigurator {
    dm::EntryType_t<decltype(ParamType)> type;
    dm::EntryType_t<decltype(DatasetForLocal)> datasetForLocal;

    static constexpr std::string_view record_name_ = "param-configurator";
    static constexpr auto record_entries_ = std::make_tuple(ParamType, DatasetForLocal);
};

}  // namespace mars2grib


// PointInTime config
namespace mars2grib {
constexpr auto PointInTimeType =       //
    dm::EntryDef<std::string>{"type"}  //
        .withDefault("default")
        .withAccessor([](auto&& v) { return &v.type; });

struct PointInTimeConfigurator {
    dm::EntryType_t<decltype(PointInTimeType)> type;

    static constexpr std::string_view record_name_ = "point-in-time-configurator";
    static constexpr auto record_entries_ = std::make_tuple(PointInTimeType);
};

}  // namespace mars2grib


// TimeRange config
namespace mars2grib {
constexpr auto TimeRangeTypeEntry =                //
    dm::EntryDef<sections::TimeRangeType>{"type"}  //
        .withAccessor([](auto&& v) { return &v.type; });
constexpr auto TypeOfStatisticalProcessing =                                         //
    dm::EntryDef<dm::TypeOfStatisticalProcessing>{"type-of-statistical-processing"}  //
        .withAccessor([](auto&& v) { return &v.typeOfStatisticalProcessing; });
constexpr auto OverallLengthOfTimeRange =                     //
    dm::EntryDef<std::string>{"overall-length-of-timerange"}  //
        .tagOptional()                                        //
        .withAccessor([](auto&& v) { return &v.overallLengthOfTimeRange; });

struct TimeRangeConfigurator {
    dm::EntryType_t<decltype(TimeRangeTypeEntry)> type;
    dm::EntryType_t<decltype(TypeOfStatisticalProcessing)> typeOfStatisticalProcessing;
    dm::EntryType_t<decltype(OverallLengthOfTimeRange)> overallLengthOfTimeRange;

    static constexpr std::string_view record_name_ = "time-statistics-configurator";
    static constexpr auto record_entries_
        = std::make_tuple(TimeRangeTypeEntry, TypeOfStatisticalProcessing, OverallLengthOfTimeRange);
};

}  // namespace mars2grib


// Process config
namespace mars2grib {
constexpr auto ProcessType =           //
    dm::EntryDef<std::string>{"type"}  //
        .withDefault("default")
        .withAccessor([](auto&& v) { return &v.type; });

struct ProcessConfigurator {
    dm::EntryType_t<decltype(ProcessType)> type;

    static constexpr std::string_view record_name_ = "ensemble-configurator";
    static constexpr auto record_entries_ = std::make_tuple(ProcessType);
};

}  // namespace mars2grib


// Model config
namespace mars2grib {
constexpr auto ModelType =             //
    dm::EntryDef<std::string>{"type"}  //
        .withDefault("default")
        .withAccessor([](auto&& v) { return &v.type; });

struct ModelConfigurator {
    dm::EntryType_t<decltype(ModelType)> type;

    static constexpr std::string_view record_name_ = "model-configurator";
    static constexpr auto record_entries_ = std::make_tuple(ModelType);
};
}  // namespace mars2grib


// Random patterns config
namespace mars2grib {
constexpr auto RandomPatternsType =    //
    dm::EntryDef<std::string>{"type"}  //
        .withDefault("default")
        .withAccessor([](auto&& v) { return &v.type; });

struct RandomPatternsConfigurator {
    dm::EntryType_t<decltype(RandomPatternsType)> type;

    static constexpr std::string_view record_name_ = "random-patterns-configurator";
    static constexpr auto record_entries_ = std::make_tuple(RandomPatternsType);
};
}  // namespace mars2grib


// Chem config
namespace mars2grib {
constexpr auto ChemType =              //
    dm::EntryDef<std::string>{"type"}  //
        .withDefault("chemical")
        .withAccessor([](auto&& v) { return &v.type; });

struct ChemConfigurator {
    dm::EntryType_t<decltype(ChemType)> type;

    static constexpr std::string_view record_name_ = "chemistry-configurator";
    static constexpr auto record_entries_ = std::make_tuple(ChemType);
};
}  // namespace mars2grib


// Directions frequencies config
namespace mars2grib {
constexpr auto DirFreqType =           //
    dm::EntryDef<std::string>{"type"}  //
        .withDefault("default")
        .withAccessor([](auto&& v) { return &v.type; });

struct DirFreqConfigurator {
    dm::EntryType_t<decltype(DirFreqType)> type;

    static constexpr std::string_view record_name_ = "directions-frequencies-configurator";
    static constexpr auto record_entries_ = std::make_tuple(DirFreqType);
};
}  // namespace mars2grib


// Satellite frequencies config
namespace mars2grib {
constexpr auto SatelliteType =         //
    dm::EntryDef<std::string>{"type"}  //
        .withDefault("default")
        .withAccessor([](auto&& v) { return &v.type; });

struct SatelliteConfigurator {
    dm::EntryType_t<decltype(SatelliteType)> type;

    static constexpr std::string_view record_name_ = "satellite-configurator";
    static constexpr auto record_entries_ = std::make_tuple(SatelliteType);
};
}  // namespace mars2grib


// Period frequencies config
namespace mars2grib {
constexpr auto PeriodType =            //
    dm::EntryDef<std::string>{"type"}  //
        .withDefault("default")
        .withAccessor([](auto&& v) { return &v.type; });

struct PeriodConfigurator {
    dm::EntryType_t<decltype(PeriodType)> type;

    static constexpr std::string_view record_name_ = "period-configurator";
    static constexpr auto record_entries_ = std::make_tuple(PeriodType);
};
}  // namespace mars2grib


namespace mars2grib {

constexpr auto ProductTemplateNumber =             //
    dm::EntryDef<std::int64_t>{"template-number"}  //
        .withAccessor([](auto&& v) { return &v.templateNumber; });


constexpr auto NestedPDTCat =              //
    dm::nestedOptRecord<rules::PDTCat>().  //
    withAccessor([](auto&& v) { return &v.pdtCat; });

// Param and Model are the only required configurators
// All others are optional/product dependent
constexpr auto NestedParam =                //
    dm::nestedRecord<ParamConfigurator>().  //
    withAccessor([](auto&& v) { return &v.param; });
constexpr auto NestedModel =                //
    dm::nestedRecord<ModelConfigurator>().  //
    withAccessor([](auto&& v) { return &v.model; });

constexpr auto NestedPointInTime =                   //
    dm::nestedOptRecord<PointInTimeConfigurator>().  //
    withAccessor([](auto&& v) { return &v.pointInTime; });
constexpr auto NestedTimeRange =                   //
    dm::nestedOptRecord<TimeRangeConfigurator>().  //
    withAccessor([](auto&& v) { return &v.timeRange; });

using multio::mars2grib::sections::LevelConfigurator;

constexpr auto NestedLevel =                   //
    dm::nestedOptRecord<LevelConfigurator>().  //
    withAccessor([](auto&& v) { return &v.level; });

constexpr auto NestedProcess =                   //
    dm::nestedOptRecord<ProcessConfigurator>().  //
    withAccessor([](auto&& v) { return &v.process; });
constexpr auto NestedRandomPatterns =                   //
    dm::nestedOptRecord<RandomPatternsConfigurator>().  //
    withAccessor([](auto&& v) { return &v.randomPatterns; });
constexpr auto NestedChemical =               //
    dm::nestedOptRecord<ChemConfigurator>().  //
    withAccessor([](auto&& v) { return &v.chemical; });
constexpr auto NestedDirFreq =                   //
    dm::nestedOptRecord<DirFreqConfigurator>().  //
    withAccessor([](auto&& v) { return &v.dirFreq; });
constexpr auto NestedPeriodRange =              //
    dm::nestedOptRecord<PeriodConfigurator>().  //
    withAccessor([](auto&& v) { return &v.periodRange; });
constexpr auto NestedSatellite =                   //
    dm::nestedOptRecord<SatelliteConfigurator>().  //
    withAccessor([](auto&& v) { return &v.satellite; });


struct ProductSection {
    dm::EntryType_t<decltype(ProductTemplateNumber)> templateNumber;

    dm::EntryType_t<decltype(NestedPDTCat)> pdtCat;
    dm::EntryType_t<decltype(NestedParam)> param;
    dm::EntryType_t<decltype(NestedModel)> model;
    dm::EntryType_t<decltype(NestedPointInTime)> pointInTime;
    dm::EntryType_t<decltype(NestedTimeRange)> timeRange;
    dm::EntryType_t<decltype(NestedLevel)> level;
    dm::EntryType_t<decltype(NestedProcess)> process;
    dm::EntryType_t<decltype(NestedRandomPatterns)> randomPatterns;
    dm::EntryType_t<decltype(NestedChemical)> chemical;
    dm::EntryType_t<decltype(NestedDirFreq)> dirFreq;
    dm::EntryType_t<decltype(NestedPeriodRange)> periodRange;
    dm::EntryType_t<decltype(NestedSatellite)> satellite;

    static constexpr std::string_view record_name_ = "product-definition-section";
    static constexpr auto record_entries_
        = std::make_tuple(ProductTemplateNumber, NestedPDTCat, NestedParam, NestedModel, NestedPointInTime,
                          NestedTimeRange, NestedLevel, NestedProcess, NestedLevel, NestedRandomPatterns,
                          NestedChemical, NestedDirFreq, NestedPeriodRange, NestedSatellite);
};
}  // namespace mars2grib


namespace datamod {
template <>
struct ApplyRecordDefaults<mars2grib::ProductSection> {
    static void applyDefaults(mars2grib::ProductSection& product) {
        using namespace mars2grib;
        using namespace mars2grib::rules;

        // Checking PDT
        {
            if (product.pdtCat.has()) {
                auto pdtNum = InferPdt<>{}.inferProductDefinitionTemplateNumber(product.pdtCat.get());

                if (product.templateNumber.has() && product.templateNumber.get() != pdtNum) {
                    std::ostringstream oss;
                    oss << "ProductSection configuration has a template number and PDT categories specified, but the "
                           "generated PDT "
                        << pdtNum << " is different from the passed " << product.templateNumber.get();
                    throw Mars2GribException(oss.str(), Here());
                }
                product.templateNumber.set(pdtNum);
            }
        }
    }
};

template <>
struct ValidateRecord<mars2grib::ProductSection> {
    static void validate(mars2grib::ProductSection& product) {
        using namespace mars2grib;
        using namespace mars2grib::rules;

        // Checking PDT
        {
            if (product.templateNumber.isUnset() && product.pdtCat.isUnset()) {
                std::ostringstream oss;
                oss << "ProductSection configuration has no template number and no PDT categories  specified.";
                throw Mars2GribException(oss.str(), Here());
            }
        }

        // Checking Time
        {
            if (product.timeRange.has() && product.pointInTime.has()) {
                std::ostringstream oss;
                oss << "ProductSection configuration has a PointInTime and a TimeStatistics section." << std::endl;
                throw Mars2GribException(oss.str(), Here());
            }
            if (product.timeRange.isUnset() && product.pointInTime.isUnset()) {
                std::ostringstream oss;
                oss << "ProductSection configuration has no time definition." << std::endl;
                throw Mars2GribException(oss.str(), Here());
            }
        }
    }
};
}  // namespace datamod

//-----------------------------------------------------------------------------
// Section5 Data representation
//-----------------------------------------------------------------------------

namespace mars2grib {

constexpr auto DataRepresTemplateNumber =          //
    dm::EntryDef<std::int64_t>{"template-number"}  //
        .withDefault(0)
        .withAccessor([](auto&& v) { return &v.templateNumber; });

struct DataRepresSection {
    dm::EntryType_t<decltype(DataRepresTemplateNumber)> templateNumber;

    static constexpr std::string_view record_name_ = "data-representation-section";
    static constexpr auto record_entries_ = std::make_tuple(DataRepresTemplateNumber);
};

}  // namespace mars2grib


//-----------------------------------------------------------------------------
// All sections
//-----------------------------------------------------------------------------

namespace mars2grib {
constexpr auto SectionsConfType =      //
    dm::EntryDef<std::string>{"type"}  //
        .withDefault("grib2")
        .withAccessor([](auto&& v) { return &v.type; });


constexpr auto NestedIndicator =          //
    dm::nestedRecord<IndicatorSection>()  //
        .withAccessor([](auto&& v) { return &v.indicator; });
constexpr auto NestedIdentification =          //
    dm::nestedRecord<IdentificationSection>()  //
        .withAccessor([](auto&& v) { return &v.identification; });
constexpr auto NestedLocalUse =          //
    dm::nestedRecord<LocalUseSection>()  //
        .withAccessor([](auto&& v) { return &v.localUse; });
constexpr auto NestedGrid =          //
    dm::nestedRecord<GridSection>()  //
        .withAccessor([](auto&& v) { return &v.grid; });
constexpr auto NestedProduct =          //
    dm::nestedRecord<ProductSection>()  //
        .withAccessor([](auto&& v) { return &v.product; });
constexpr auto NestedDataRepres =          //
    dm::nestedRecord<DataRepresSection>()  //
        .withAccessor([](auto&& v) { return &v.dataRepres; });

struct SectionsConf {
    dm::EntryType_t<decltype(SectionsConfType)> type;

    dm::EntryType_t<decltype(NestedIndicator)> indicator;
    dm::EntryType_t<decltype(NestedIdentification)> identification;
    dm::EntryType_t<decltype(NestedLocalUse)> localUse;
    dm::EntryType_t<decltype(NestedGrid)> grid;
    dm::EntryType_t<decltype(NestedProduct)> product;
    dm::EntryType_t<decltype(NestedDataRepres)> dataRepres;

    static constexpr std::string_view record_name_ = "encoder";
    static constexpr auto record_entries_
        = std::make_tuple(SectionsConfType, NestedIndicator, NestedIdentification, NestedLocalUse, NestedGrid,
                          NestedProduct, NestedDataRepres);
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


template <>
struct TypeToString<multio::mars2grib::IndicatorSection> {
    std::string operator()() const { return std::string("mars2grib::sections::IndicatorSection"); };
};
template <>
struct TypeToString<multio::mars2grib::OriginConfigurator> {
    std::string operator()() const { return std::string("mars2grib::sections::OriginConfigurator"); };
};
template <>
struct TypeToString<multio::mars2grib::DataTypeConfigurator> {
    std::string operator()() const { return std::string("mars2grib::sections::DataTypeConfigurator"); };
};
template <>
struct TypeToString<multio::mars2grib::ReferenceTimeConfigurator> {
    std::string operator()() const { return std::string("mars2grib::sections::ReferenceTimeConfigurator"); };
};
template <>
struct TypeToString<multio::mars2grib::TablesConfigurator> {
    std::string operator()() const { return std::string("mars2grib::sections::TablesConfigurator"); };
};
template <>
struct TypeToString<multio::mars2grib::IdentificationSection> {
    std::string operator()() const { return std::string("mars2grib::sections::IdentificationSection"); };
};
template <>
struct TypeToString<multio::mars2grib::LocalUseSection> {
    std::string operator()() const { return std::string("mars2grib::sections::LocalUseSection"); };
};
template <>
struct TypeToString<multio::mars2grib::GridSection> {
    std::string operator()() const { return std::string("mars2grib::sections::GridSection"); };
};
template <>
struct TypeToString<multio::mars2grib::ParamConfigurator> {
    std::string operator()() const { return std::string("mars2grib::sections::ParamConfigurator"); };
};
template <>
struct TypeToString<multio::mars2grib::PointInTimeConfigurator> {
    std::string operator()() const { return std::string("mars2grib::sections::PointInTimeConfigurator"); };
};
template <>
struct TypeToString<multio::mars2grib::TimeRangeConfigurator> {
    std::string operator()() const { return std::string("mars2grib::sections::TimeRangeConfigurator"); };
};
template <>
struct TypeToString<multio::mars2grib::ProcessConfigurator> {
    std::string operator()() const { return std::string("mars2grib::sections::ProcessConfigurator"); };
};
template <>
struct TypeToString<multio::mars2grib::ModelConfigurator> {
    std::string operator()() const { return std::string("mars2grib::sections::ModelConfigurator"); };
};
template <>
struct TypeToString<multio::mars2grib::RandomPatternsConfigurator> {
    std::string operator()() const { return std::string("mars2grib::sections::RandomPatternsConfigurator"); };
};
template <>
struct TypeToString<multio::mars2grib::SatelliteConfigurator> {
    std::string operator()() const { return std::string("mars2grib::sections::SatelliteConfigurator"); };
};
template <>
struct TypeToString<multio::mars2grib::ChemConfigurator> {
    std::string operator()() const { return std::string("mars2grib::sections::ChemConfigurator"); };
};
template <>
struct TypeToString<multio::mars2grib::DirFreqConfigurator> {
    std::string operator()() const { return std::string("mars2grib::sections::DirFreqConfigurator"); };
};
template <>
struct TypeToString<multio::mars2grib::PeriodConfigurator> {
    std::string operator()() const { return std::string("mars2grib::sections::PeriodConfigurator"); };
};
template <>
struct TypeToString<multio::mars2grib::ProductSection> {
    std::string operator()() const { return std::string("mars2grib::sections::ProductSection"); };
};
template <>
struct TypeToString<multio::mars2grib::DataRepresSection> {
    std::string operator()() const { return std::string("mars2grib::sections::DataRepresSection"); };
};
template <>
struct TypeToString<multio::mars2grib::SectionsConf> {
    std::string operator()() const { return std::string("mars2grib::sections::SectionsConf"); };
};

}  // namespace util

}  // namespace multio

