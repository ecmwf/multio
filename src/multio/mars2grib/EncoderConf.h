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
#include "multio/datamod/DataModelling.h"
#include "multio/datamod/GribKeys.h"
#include "multio/datamod/GribTypes.h"
#include "multio/datamod/ReaderWriter.h"
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

//-----------------------------------------------------------------------------
// Section0
//-----------------------------------------------------------------------------

namespace mars2grib {
enum class EncoderIndicatorDef : std::uint64_t
{
    TemplateNumber,
};
}

namespace datamod {
using mars2grib::EncoderIndicatorDef;

MULTIO_KEY_SET_DESCRIPTION(EncoderIndicatorDef,  //
                           "indicator-section",  //
                                                 //
                           KeyDef<EncoderIndicatorDef::TemplateNumber, std::int64_t>{"template-number"}.withDefault(0))
};  // namespace datamod

//-----------------------------------------------------------------------------
// Section1
//-----------------------------------------------------------------------------

// Origin configurator
namespace mars2grib {
enum class EncoderOriginDef : std::uint64_t
{
    Type,
    SubCentre,
};
}

namespace datamod {
using mars2grib::EncoderOriginDef;
MULTIO_KEY_SET_DESCRIPTION(EncoderOriginDef,                                                                //
                           "origin-configurator",                                                           //
                                                                                                            //
                           KeyDef<EncoderOriginDef::Type, std::string>{"type"}.withDefault("default"),      //
                           KeyDef<EncoderOriginDef::SubCentre, std::int64_t>{"sub-centre"}.withDefault(0))  //
};  // namespace datamod


// DataType configurator
namespace mars2grib {
enum class EncoderDataTypeDef : std::uint64_t
{
    Type,
};
}

namespace datamod {
using mars2grib::EncoderDataTypeDef;
MULTIO_KEY_SET_DESCRIPTION(EncoderDataTypeDef,                                                            //
                           "data-type-configurator",                                                      //
                                                                                                          //
                           KeyDef<EncoderDataTypeDef::Type, std::string>{"type"}.withDefault("default"))  //
};  // namespace datamod


// ReferenceTime configurator
namespace mars2grib {
enum class EncoderReferenceTimeDef : std::uint64_t
{
    Type,
};
}

namespace datamod {
using mars2grib::EncoderReferenceTimeDef;
MULTIO_KEY_SET_DESCRIPTION(EncoderReferenceTimeDef,                                                            //
                           "reference-time-configurator",                                                      //
                                                                                                               //
                           KeyDef<EncoderReferenceTimeDef::Type, std::string>{"type"}.withDefault("default"))  //
};  // namespace datamod


// Tables configurator
namespace mars2grib {
enum class EncoderTablesDef : std::uint64_t
{
    Type,
    TablesVersion,
    LocalTablesVersion
};
}

namespace datamod {
using mars2grib::EncoderTablesDef;
MULTIO_KEY_SET_DESCRIPTION(
    EncoderTablesDef,                                                                                   //
    "tables-configurator",                                                                              //
                                                                                                        //
    KeyDef<EncoderTablesDef::Type, std::string>{"type"}.withDefault("default"),                         //
    KeyDef<EncoderTablesDef::TablesVersion, std::int64_t>{"tables-version"}.tagOptional(),              //
    KeyDef<EncoderTablesDef::LocalTablesVersion, std::int64_t>{"local-tables-version"}.withDefault(0))  //
};  // namespace datamod

// Whole identification section
namespace mars2grib {
enum class EncoderIdentificationDef : std::uint64_t
{
    TemplateNumber,
    Origin,
    DataType,
    ReferenceTime,
    Tables
};
}


namespace datamod {
using mars2grib::EncoderIdentificationDef;
MULTIO_KEY_SET_DESCRIPTION(
    EncoderIdentificationDef,                                                                          //
    "identification-section",                                                                          //
                                                                                                       //
    KeyDef<EncoderIdentificationDef::TemplateNumber, std::int64_t>{"template-number"}.withDefault(0),  //
    nestedKeyDef<EncoderIdentificationDef::Origin, EncoderOriginDef>(),                                //
    nestedKeyDef<EncoderIdentificationDef::DataType, EncoderDataTypeDef>(),                            //
    nestedKeyDef<EncoderIdentificationDef::ReferenceTime, EncoderReferenceTimeDef>(),                  //
    nestedKeyDef<EncoderIdentificationDef::Tables, EncoderTablesDef>())
};  // namespace datamod

//-----------------------------------------------------------------------------
// Section2 Local use
//-----------------------------------------------------------------------------

namespace mars2grib {
enum class EncoderLocalUseDef : std::uint64_t
{
    TemplateNumber,
};
}


namespace datamod {
using mars2grib::EncoderLocalUseDef;
MULTIO_KEY_SET_DESCRIPTION(
    EncoderLocalUseDef,                                                                          //
    "local-use-section",                                                                         //
                                                                                                 //
    KeyDef<EncoderLocalUseDef::TemplateNumber, std::int64_t>{"template-number"}.withDefault(0))  //
};  // namespace datamod


//-----------------------------------------------------------------------------
// Section3 Grid
//-----------------------------------------------------------------------------

namespace mars2grib {
enum class EncoderGridDef : std::uint64_t
{
    TemplateNumber,
};
}


namespace datamod {
using mars2grib::EncoderGridDef;
MULTIO_KEY_SET_DESCRIPTION(EncoderGridDef,                                                                          //
                           "grid-definition-section",                                                               //
                                                                                                                    //
                           KeyDef<EncoderGridDef::TemplateNumber, std::int64_t>{"template-number"}.withDefault(0))  //
};  // namespace datamod


//-----------------------------------------------------------------------------
// Section4 Product definition
//-----------------------------------------------------------------------------

// Param config
namespace mars2grib {
enum class EncoderParamDef : std::uint64_t
{
    Type,
    DatasetForLocal,
};
}

namespace datamod {
using mars2grib::EncoderParamDef;
MULTIO_KEY_SET_DESCRIPTION(EncoderParamDef,                                                                           //
                           "param-configurator",                                                                      //
                                                                                                                      //
                           KeyDef<EncoderParamDef::Type, std::string>{"type"}.withDefault("paramId"),                 //
                           KeyDef<EncoderParamDef::DatasetForLocal, std::string>{"dataset-for-local"}.tagOptional())  //
};  // namespace datamod


// PointInTime config
namespace mars2grib {
enum class EncoderPointInTimeDef : std::uint64_t
{
    Type,
};
}

namespace datamod {
using mars2grib::EncoderPointInTimeDef;
MULTIO_KEY_SET_DESCRIPTION(EncoderPointInTimeDef,                                                            //
                           "point-in-time-configurator",                                                     //
                                                                                                             //
                           KeyDef<EncoderPointInTimeDef::Type, std::string>{"type"}.withDefault("default"))  //
};  // namespace datamod


// TimeRange config
namespace mars2grib {
enum class EncoderTimeRangeDef : std::uint64_t
{
    Type,
    TypeOfStatisticalProcessing,
    OverallLengthOfTimeRange,
    // EncodeStepZero, // has been removed
};
}

namespace datamod {
using mars2grib::EncoderTimeRangeDef;
using mars2grib::sections::TimeRangeType;
MULTIO_KEY_SET_DESCRIPTION(
    EncoderTimeRangeDef,                                       //
    "time-statistics-configurator",                            //
                                                               //
    KeyDef<EncoderTimeRangeDef::Type, TimeRangeType>{"type"},  //
    KeyDef<EncoderTimeRangeDef::TypeOfStatisticalProcessing, TypeOfStatisticalProcessing>{
        "type-of-statistical-processing"},                                                                            //
    KeyDef<EncoderTimeRangeDef::OverallLengthOfTimeRange, std::string>{"overall-length-of-timerange"}.tagOptional())  //
};  // namespace datamod


// Process config
namespace mars2grib {
enum class EncoderProcessDef : std::uint64_t
{
    Type,
};
}

namespace datamod {
using mars2grib::EncoderProcessDef;
MULTIO_KEY_SET_DESCRIPTION(EncoderProcessDef,                                                            //
                           "ensemble-configurator",                                                      //
                                                                                                         //
                           KeyDef<EncoderProcessDef::Type, std::string>{"type"}.withDefault("default"))  //
};  // namespace datamod


// Model config
namespace mars2grib {
enum class EncoderModelDef : std::uint64_t
{
    Type,
};
}

namespace datamod {
using mars2grib::EncoderModelDef;
MULTIO_KEY_SET_DESCRIPTION(EncoderModelDef,                                                            //
                           "model-configurator",                                                       //
                                                                                                       //
                           KeyDef<EncoderModelDef::Type, std::string>{"type"}.withDefault("default"))  //
};  // namespace datamod

// Random patterns config
namespace mars2grib {
enum class EncoderRandomPatternsDef : std::uint64_t
{
    Type,
};
}

namespace datamod {
using mars2grib::EncoderRandomPatternsDef;
MULTIO_KEY_SET_DESCRIPTION(EncoderRandomPatternsDef,                                                            //
                           "random-patterns-configurator",                                                      //
                                                                                                                //
                           KeyDef<EncoderRandomPatternsDef::Type, std::string>{"type"}.withDefault("default"))  //
};  // namespace datamod


// Chem config
namespace mars2grib {
enum class EncoderChemDef : std::uint64_t
{
    Type,
};
}

namespace datamod {
using mars2grib::EncoderChemDef;
MULTIO_KEY_SET_DESCRIPTION(EncoderChemDef,                                                            //
                           "chemistry-configurator",                                                  //
                                                                                                      //
                           KeyDef<EncoderChemDef::Type, std::string>{"type"}.withDefault("chemical"))  //
};  // namespace datamod


// Directions frequencies config
namespace mars2grib {
enum class EncoderDirFreqDef : std::uint64_t
{
    Type,
};
}

namespace datamod {
using mars2grib::EncoderDirFreqDef;
MULTIO_KEY_SET_DESCRIPTION(EncoderDirFreqDef,                                                            //
                           "directions-frequencies-configurator",                                        //
                                                                                                         //
                           KeyDef<EncoderDirFreqDef::Type, std::string>{"type"}.withDefault("default"))  //
};  // namespace datamod


// Satellite frequencies config
namespace mars2grib {
enum class EncoderSatelliteDef : std::uint64_t
{
    Type,
};
}

namespace datamod {
using mars2grib::EncoderSatelliteDef;
MULTIO_KEY_SET_DESCRIPTION(EncoderSatelliteDef,                                                            //
                           "satellite-configurator",                                                       //
                                                                                                           //
                           KeyDef<EncoderSatelliteDef::Type, std::string>{"type"}.withDefault("default"))  //
};  // namespace datamod


// Period frequencies config
namespace mars2grib {
enum class EncoderPeriodDef : std::uint64_t
{
    Type,
};
}

namespace datamod {
using mars2grib::EncoderPeriodDef;
MULTIO_KEY_SET_DESCRIPTION(EncoderPeriodDef,                                                            //
                           "period-configurator",                                                       //
                                                                                                        //
                           KeyDef<EncoderPeriodDef::Type, std::string>{"type"}.withDefault("default"))  //
};  // namespace datamod


namespace mars2grib {
enum class EncoderProductDef : std::uint64_t
{
    TemplateNumber,
    PDTCat,  // Added optional to generate template number
    Param,
    PointInTime,
    TimeRange,
    Model,
    Process,
    Level,
    RandomPatterns,
    Chemical,
    DirFreq,
    PeriodRange,
    Satellite,
};
}


namespace datamod {
using mars2grib::EncoderProductDef;
using mars2grib::rules::PDTCatDef;
using multio::mars2grib::sections::LevelDef;
MULTIO_KEY_SET_DESCRIPTION(EncoderProductDef,             //
                           "product-definition-section",  //
                                                          //
                           KeyDef<EncoderProductDef::TemplateNumber, std::int64_t>{"template-number"}
                               .tagDefaulted(),  // Required but can be infered form PDTCat
                           nestedOptKeyDef<EncoderProductDef::PDTCat, PDTCatDef>(),                         //
                           nestedKeyDef<EncoderProductDef::Param, EncoderParamDef>(),                       //
                           nestedKeyDef<EncoderProductDef::Model, EncoderModelDef>(),                       //
                           nestedOptKeyDef<EncoderProductDef::PointInTime, EncoderPointInTimeDef>(),        //
                           nestedOptKeyDef<EncoderProductDef::TimeRange, EncoderTimeRangeDef>(),            //
                           nestedOptKeyDef<EncoderProductDef::Process, EncoderProcessDef>(),                //
                           nestedOptKeyDef<EncoderProductDef::Level, LevelDef>(),                           //
                           nestedOptKeyDef<EncoderProductDef::RandomPatterns, EncoderRandomPatternsDef>(),  //
                           nestedOptKeyDef<EncoderProductDef::Chemical, EncoderChemDef>(),                  //
                           nestedOptKeyDef<EncoderProductDef::DirFreq, EncoderDirFreqDef>(),                //
                           nestedOptKeyDef<EncoderProductDef::PeriodRange, EncoderPeriodDef>(),             //
                           nestedOptKeyDef<EncoderProductDef::Satellite, EncoderSatelliteDef>())            //


template <>
struct KeySetAlter<KeySet<EncoderProductDef>> {
    static void alter(KeyValueSet<KeySet<EncoderProductDef>>& product) {
        using namespace datamod;
        using namespace mars2grib;
        using namespace mars2grib::rules;

        // Checking PDT
        {
            const auto& pdtCat = key<EncoderProductDef::PDTCat>(product);
            auto& templateNumber = key<EncoderProductDef::TemplateNumber>(product);

            if (templateNumber.isMissing() && pdtCat.isMissing()) {
                std::ostringstream oss;
                oss << "EncoderProduct has no template number and no PDT categories  specified.";
                throw Mars2GribException(oss.str(), Here());
            }

            if (pdtCat.has()) {
                auto pdtNum = InferPdt<>{}.inferProductDefinitionTemplateNumber(pdtCat.get());

                if (templateNumber.has() && templateNumber.get() != pdtNum) {
                    std::ostringstream oss;
                    oss << "EncoderProduct has a template number and PDT categories specified, but the generated PDT "
                        << pdtNum << " is different from the passed " << templateNumber.get();
                    throw Mars2GribException(oss.str(), Here());
                }
                templateNumber.set(pdtNum);
            }
        }


        // Checking Time
        {
            const auto& timeRange = key<EncoderProductDef::TimeRange>(product);
            const auto& pointInTime = key<EncoderProductDef::PointInTime>(product);

            if (timeRange.has() && pointInTime.has()) {
                std::ostringstream oss;
                oss << "EncoderProduct has a PointInTime and a TimeStatistics section." << std::endl;
                throw Mars2GribException(oss.str(), Here());
            }
            if (timeRange.isMissing() && pointInTime.isMissing()) {
                std::ostringstream oss;
                oss << "EncoderProduct has no time definition." << std::endl;
                throw Mars2GribException(oss.str(), Here());
            }
        }
    }
};
};  // namespace datamod


//-----------------------------------------------------------------------------
// Section5 Data representation
//-----------------------------------------------------------------------------

namespace mars2grib {
enum class EncoderDataRepresDef : std::uint64_t
{
    TemplateNumber,
};
}


namespace datamod {
using mars2grib::EncoderDataRepresDef;
MULTIO_KEY_SET_DESCRIPTION(
    EncoderDataRepresDef,                                                                          //
    "data-representation-section",                                                                 //
                                                                                                   //
    KeyDef<EncoderDataRepresDef::TemplateNumber, std::int64_t>{"template-number"}.withDefault(0))  //
};  // namespace datamod


//-----------------------------------------------------------------------------
// All sections
//-----------------------------------------------------------------------------


namespace mars2grib {
enum class EncoderSectionsDef : std::uint64_t
{
    Type,
    Indicator,
    Identification,
    LocalUse,
    Grid,
    Product,
    DataRepres,
};
}


namespace datamod {
using mars2grib::EncoderSectionsDef;
MULTIO_KEY_SET_DESCRIPTION(EncoderSectionsDef,                                                            //
                           "encoder",                                                                     //
                                                                                                          //
                           KeyDef<EncoderSectionsDef::Type, std::string>{"type"}.withDefault("grib2"),    //
                           nestedKeyDef<EncoderSectionsDef::Indicator, EncoderIndicatorDef>(),            //
                           nestedKeyDef<EncoderSectionsDef::Identification, EncoderIdentificationDef>(),  //
                           nestedKeyDef<EncoderSectionsDef::LocalUse, EncoderLocalUseDef>(),              //
                           nestedKeyDef<EncoderSectionsDef::Grid, EncoderGridDef>(),                      //
                           nestedKeyDef<EncoderSectionsDef::Product, EncoderProductDef>(),                //
                           nestedKeyDef<EncoderSectionsDef::DataRepres, EncoderDataRepresDef>())
};  // namespace datamod

namespace mars2grib {

using EncoderSectionsKeySet = datamod::KeySet<EncoderSectionsDef>;
using EncoderSectionsKeyValueSet = datamod::KeyValueSet<EncoderSectionsKeySet>;
using EncoderSections = EncoderSectionsKeyValueSet;
}  // namespace mars2grib


//-----------------------------------------------------------------------------
// Outer wrapper of current configuration files... used as interface
// for migration
//-----------------------------------------------------------------------------

namespace mars2grib {
enum class EncoderInfoDef : std::uint64_t
{
    Name,
    Tag,
    Sections,  // Parsed Conf
    Sample,
};
}

namespace datamod {
using mars2grib::EncoderInfoDef;
using mars2grib::EncoderSections;
MULTIO_KEY_SET_DESCRIPTION(EncoderInfoDef,                                                //
                           "encoder-configuration",                                       //
                                                                                          //
                           KeyDef<EncoderInfoDef::Name, std::string>{"name"},             //
                           KeyDef<EncoderInfoDef::Tag, std::string>{"tag"},               //
                           nestedKeyDef<EncoderInfoDef::Sections, EncoderSectionsDef>(),  //
                           KeyDef<EncoderInfoDef::Sample, std::string>{"sample"}.tagOptional())


};  // namespace datamod


namespace mars2grib {

using EncoderInfoKeySet = datamod::KeySet<EncoderInfoDef>;
using EncoderInfoKeyValueSet = datamod::KeyValueSet<EncoderInfoKeySet>;
using EncoderInfo = EncoderInfoKeyValueSet;

//---------------------------------------------------------------------------------------------------------------------

}  // namespace mars2grib
}  // namespace multio

