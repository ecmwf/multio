/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/// @author Philipp Geier

/// @date Oct 2025

#pragma once

#include "eckit/config/LocalConfiguration.h"
#include "multio/action/encode-mtg2/EncodeMtg2Exception.h"
#include "multio/datamod/ContainerInterop.h"
#include "multio/datamod/DataModelling.h"

#include <memory>


namespace multio {

//-----------------------------------------------------------------------------
// Detailed descriptions of the encoder configuration.
// First attempt is to replicate existing structure and to migrate the rule
// search mechanism while keeping the fortran encoders.
// Often the structures of the configs have a subconfiguration where to only available key-value is "type": "default"
// These have to be refactored to something less but more expressive
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// Section0
//-----------------------------------------------------------------------------

namespace action {
enum class EncoderIndicatorDef : std::uint64_t
{
    TemplateNumber,
};
}

namespace datamod {
using action::EncoderIndicatorDef;

MULTIO_KEY_SET_DESCRIPTION(EncoderIndicatorDef,  //
                           "indicator-section",  //
                                                 //
                           KeyDef<EncoderIndicatorDef::TemplateNumber, std::int64_t>{"template-number"}.withDefault(0))
};  // namespace datamod

//-----------------------------------------------------------------------------
// Section1
//-----------------------------------------------------------------------------

// Origin configurator
namespace action {
enum class EncoderOriginDef : std::uint64_t
{
    Type,
    SubCentre,
};
}

namespace datamod {
using action::EncoderOriginDef;
MULTIO_KEY_SET_DESCRIPTION(EncoderOriginDef,                                                                //
                           "origin-configurator",                                                           //
                                                                                                            //
                           KeyDef<EncoderOriginDef::Type, std::string>{"type"}.withDefault("default"),      //
                           KeyDef<EncoderOriginDef::SubCentre, std::int64_t>{"sub-centre"}.withDefault(0))  //
};  // namespace datamod


// DataType configurator
namespace action {
enum class EncoderDataTypeDef : std::uint64_t
{
    Type,
};
}

namespace datamod {
using action::EncoderDataTypeDef;
MULTIO_KEY_SET_DESCRIPTION(EncoderDataTypeDef,                                                            //
                           "data-type-configurator",                                                      //
                                                                                                          //
                           KeyDef<EncoderDataTypeDef::Type, std::string>{"type"}.withDefault("default"))  //
};  // namespace datamod


// ReferenceTime configurator
namespace action {
enum class EncoderReferenceTimeDef : std::uint64_t
{
    Type,
};
}

namespace datamod {
using action::EncoderReferenceTimeDef;
MULTIO_KEY_SET_DESCRIPTION(EncoderReferenceTimeDef,                                                            //
                           "reference-time-configurator",                                                      //
                                                                                                               //
                           KeyDef<EncoderReferenceTimeDef::Type, std::string>{"type"}.withDefault("default"))  //
};  // namespace datamod


// Tables configurator
namespace action {
enum class EncoderTablesDef : std::uint64_t
{
    Type,
    TablesVersion,
    LocalTablesVersion
};
}

namespace datamod {
using action::EncoderTablesDef;
MULTIO_KEY_SET_DESCRIPTION(
    EncoderTablesDef,                                                                                   //
    "tables-configurator",                                                                              //
                                                                                                        //
    KeyDef<EncoderTablesDef::Type, std::string>{"type"}.withDefault("default"),                         //
    KeyDef<EncoderTablesDef::TablesVersion, std::int64_t>{"tables-version"}.tagOptional(),              //
    KeyDef<EncoderTablesDef::LocalTablesVersion, std::int64_t>{"local-tables-version"}.withDefault(0))  //
};  // namespace datamod

// Whole identification section
namespace action {
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
using action::EncoderIdentificationDef;
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

namespace action {
enum class EncoderLocalUseDef : std::uint64_t
{
    TemplateNumber,
};
}


namespace datamod {
using action::EncoderLocalUseDef;
MULTIO_KEY_SET_DESCRIPTION(
    EncoderLocalUseDef,                                                                          //
    "local-use-section",                                                                         //
                                                                                                 //
    KeyDef<EncoderLocalUseDef::TemplateNumber, std::int64_t>{"template-number"}.withDefault(0))  //
};  // namespace datamod


//-----------------------------------------------------------------------------
// Section3 Grid
//-----------------------------------------------------------------------------

namespace action {
enum class EncoderGridDef : std::uint64_t
{
    TemplateNumber,
};
}


namespace datamod {
using action::EncoderGridDef;
MULTIO_KEY_SET_DESCRIPTION(EncoderGridDef,                                                                          //
                           "grid-definition-section",                                                               //
                                                                                                                    //
                           KeyDef<EncoderGridDef::TemplateNumber, std::int64_t>{"template-number"}.withDefault(0))  //
};  // namespace datamod


//-----------------------------------------------------------------------------
// Section4 Product definition
//-----------------------------------------------------------------------------

// Param config
namespace action {
enum class EncoderParamDef : std::uint64_t
{
    Type,
    DatasetForLocal,
};
}

namespace datamod {
using action::EncoderParamDef;
MULTIO_KEY_SET_DESCRIPTION(EncoderParamDef,                                                                           //
                           "param-configurator",                                                                      //
                                                                                                                      //
                           KeyDef<EncoderParamDef::Type, std::string>{"type"}.withDefault("paramId"),                 //
                           KeyDef<EncoderParamDef::DatasetForLocal, std::string>{"dataset-for-local"}.tagOptional())  //
};  // namespace datamod


// PointInTime config
namespace action {
enum class EncoderPointInTimeDef : std::uint64_t
{
    Type,
};
}

namespace datamod {
using action::EncoderPointInTimeDef;
MULTIO_KEY_SET_DESCRIPTION(EncoderPointInTimeDef,                                                            //
                           "point-in-time-configurator",                                                     //
                                                                                                             //
                           KeyDef<EncoderPointInTimeDef::Type, std::string>{"type"}.withDefault("default"))  //
};  // namespace datamod


// TimeRange config
namespace action {
enum class EncoderTimeRangeDef : std::uint64_t
{
    Type,
    TypeOfStatisticalProcessing,
    OverallLengthOfTimeRange,
    // EncodeStepZero, // has been removed
};
}

namespace datamod {
using action::EncoderTimeRangeDef;
MULTIO_KEY_SET_DESCRIPTION(
    EncoderTimeRangeDef,                                                                                              //
    "time-statistics-configurator",                                                                                   //
                                                                                                                      //
    KeyDef<EncoderTimeRangeDef::Type, std::string>{"type"},                                                           //
    KeyDef<EncoderTimeRangeDef::TypeOfStatisticalProcessing, std::string>{"type-of-statistical-processing"},          //
    KeyDef<EncoderTimeRangeDef::OverallLengthOfTimeRange, std::string>{"overall-length-of-timerange"}.tagOptional())  //
};  // namespace datamod


// Process config
namespace action {
enum class EncoderProcessDef : std::uint64_t
{
    Type,
};
}

namespace datamod {
using action::EncoderProcessDef;
MULTIO_KEY_SET_DESCRIPTION(EncoderProcessDef,                                                            //
                           "ensemble-configurator",                                                      //
                                                                                                         //
                           KeyDef<EncoderProcessDef::Type, std::string>{"type"}.withDefault("default"))  //
};  // namespace datamod


// Model config
namespace action {
enum class EncoderModelDef : std::uint64_t
{
    Type,
};
}

namespace datamod {
using action::EncoderModelDef;
MULTIO_KEY_SET_DESCRIPTION(EncoderModelDef,                                                            //
                           "model-configurator",                                                       //
                                                                                                       //
                           KeyDef<EncoderModelDef::Type, std::string>{"type"}.withDefault("default"))  //
};  // namespace datamod

// Level config
namespace action {
enum class EncoderLevelDef : std::uint64_t
{
    Type,
};
}

namespace datamod {
using action::EncoderLevelDef;
MULTIO_KEY_SET_DESCRIPTION(EncoderLevelDef,                                                            //
                           "level-configurator",                                                       //
                                                                                                       //
                           KeyDef<EncoderLevelDef::Type, std::string>{"type"}.withDefault("default"))  //
};  // namespace datamod

// Random patterns config
namespace action {
enum class EncoderRandomPatternsDef : std::uint64_t
{
    Type,
};
}

namespace datamod {
using action::EncoderRandomPatternsDef;
MULTIO_KEY_SET_DESCRIPTION(EncoderRandomPatternsDef,                                                            //
                           "random-patterns-configurator",                                                      //
                                                                                                                //
                           KeyDef<EncoderRandomPatternsDef::Type, std::string>{"type"}.withDefault("default"))  //
};  // namespace datamod


// Chem config
namespace action {
enum class EncoderChemDef : std::uint64_t
{
    Type,
};
}

namespace datamod {
using action::EncoderChemDef;
MULTIO_KEY_SET_DESCRIPTION(EncoderChemDef,                                                            //
                           "chemistry-configurator",                                                  //
                                                                                                      //
                           KeyDef<EncoderChemDef::Type, std::string>{"type"}.withDefault("default"))  //
};  // namespace datamod


// Directions frequencies config
namespace action {
enum class EncoderDirFreqDef : std::uint64_t
{
    Type,
};
}

namespace datamod {
using action::EncoderDirFreqDef;
MULTIO_KEY_SET_DESCRIPTION(EncoderDirFreqDef,                                                            //
                           "directions-frequencies-configurator",                                        //
                                                                                                         //
                           KeyDef<EncoderDirFreqDef::Type, std::string>{"type"}.withDefault("default"))  //
};  // namespace datamod


// Satellite frequencies config
namespace action {
enum class EncoderSatelliteDef : std::uint64_t
{
    Type,
};
}

namespace datamod {
using action::EncoderSatelliteDef;
MULTIO_KEY_SET_DESCRIPTION(EncoderSatelliteDef,                                                            //
                           "satellite-configurator",                                                       //
                                                                                                           //
                           KeyDef<EncoderSatelliteDef::Type, std::string>{"type"}.withDefault("default"))  //
};  // namespace datamod


// Period frequencies config
namespace action {
enum class EncoderPeriodDef : std::uint64_t
{
    Type,
};
}

namespace datamod {
using action::EncoderPeriodDef;
MULTIO_KEY_SET_DESCRIPTION(EncoderPeriodDef,                                                            //
                           "period-configurator",                                                       //
                                                                                                        //
                           KeyDef<EncoderPeriodDef::Type, std::string>{"type"}.withDefault("default"))  //
};  // namespace datamod


namespace action {
enum class EncoderProductDef : std::uint64_t
{
    TemplateNumber,  // Todo create own PDT type
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
using action::EncoderProductDef;
MULTIO_KEY_SET_DESCRIPTION(
    EncoderProductDef,                                                                          //
    "product-definition-section",                                                               //
                                                                                                //
    KeyDef<EncoderProductDef::TemplateNumber, std::int64_t>{"template-number"},                 //
    nestedKeyDef<EncoderProductDef::Param, EncoderParamDef>(),                                  //
    nestedKeyDef<EncoderProductDef::Model, EncoderModelDef>(),                                  //
    nestedKeyDef<EncoderProductDef::PointInTime, EncoderPointInTimeDef>().tagOptional(),        //
    nestedKeyDef<EncoderProductDef::TimeRange, EncoderTimeRangeDef>().tagOptional(),            //
    nestedKeyDef<EncoderProductDef::Process, EncoderProcessDef>().tagOptional(),                //
    nestedKeyDef<EncoderProductDef::Level, EncoderLevelDef>().tagOptional(),                    //
    nestedKeyDef<EncoderProductDef::RandomPatterns, EncoderRandomPatternsDef>().tagOptional(),  //
    nestedKeyDef<EncoderProductDef::Chemical, EncoderChemDef>().tagOptional(),                  //
    nestedKeyDef<EncoderProductDef::DirFreq, EncoderDirFreqDef>().tagOptional(),                //
    nestedKeyDef<EncoderProductDef::PeriodRange, EncoderPeriodDef>().tagOptional(),             //
    nestedKeyDef<EncoderProductDef::Satellite, EncoderSatelliteDef>().tagOptional())            //


template <>
struct KeySetAlter<KeySet<EncoderProductDef>> {
    static void alter(KeyValueSet<KeySet<EncoderProductDef>>& product) {
        using namespace datamod;

        const auto& timeRange = key<EncoderProductDef::TimeRange>(product);
        const auto& pointInTime = key<EncoderProductDef::PointInTime>(product);

        if (timeRange.has() && pointInTime.has()) {
            std::ostringstream oss;
            oss << "EncoderProduct has a PointInTime and a TimeStatistics section." << std::endl;
            throw action::EncodeMtg2Exception(oss.str(), Here());
        }
        if (timeRange.isMissing() && pointInTime.isMissing()) {
            std::ostringstream oss;
            oss << "EncoderProduct has no time definition." << std::endl;
            throw action::EncodeMtg2Exception(oss.str(), Here());
        }
    }
};
};  // namespace datamod


//-----------------------------------------------------------------------------
// Section5 Data representation
//-----------------------------------------------------------------------------

namespace action {
enum class EncoderDataRepresDef : std::uint64_t
{
    TemplateNumber,
};
}


namespace datamod {
using action::EncoderDataRepresDef;
MULTIO_KEY_SET_DESCRIPTION(
    EncoderDataRepresDef,                                                                          //
    "data-representation-section",                                                                 //
                                                                                                   //
    KeyDef<EncoderDataRepresDef::TemplateNumber, std::int64_t>{"template-number"}.withDefault(0))  //
};  // namespace datamod


//-----------------------------------------------------------------------------
// All sections
//-----------------------------------------------------------------------------


namespace action {
enum class EncoderSectionsDef : std::uint64_t
{
    Indicator,
    Identification,
    LocalUse,
    Grid,
    Product,
    DataRepres,
};
}


namespace datamod {
using action::EncoderSectionsDef;
MULTIO_KEY_SET_DESCRIPTION(EncoderSectionsDef,                                                            //
                           "encoder",                                                                     //
                                                                                                          //
                           nestedKeyDef<EncoderSectionsDef::Indicator, EncoderIndicatorDef>(),            //
                           nestedKeyDef<EncoderSectionsDef::Identification, EncoderIdentificationDef>(),  //
                           nestedKeyDef<EncoderSectionsDef::LocalUse, EncoderLocalUseDef>(),              //
                           nestedKeyDef<EncoderSectionsDef::Grid, EncoderGridDef>(),                      //
                           nestedKeyDef<EncoderSectionsDef::Product, EncoderProductDef>(),                //
                           nestedKeyDef<EncoderSectionsDef::DataRepres, EncoderDataRepresDef>())
};  // namespace datamod

namespace action {

using EncoderSectionsKeySet = datamod::KeySet<EncoderSectionsDef>;
using EncoderSectionsKeyValueSet = datamod::KeyValueSet<EncoderSectionsKeySet>;
using EncoderSections = EncoderSectionsKeyValueSet;
}  // namespace action


//-----------------------------------------------------------------------------
// Outer wrapper of current configuration files... used as interface
// for migration
//-----------------------------------------------------------------------------

namespace action {
enum class EncoderInfoDef : std::uint64_t
{
    Name,
    Tag,
    Sections,  // Parsed Conf
    Sample,
};
}

namespace datamod {
using action::EncoderInfoDef;
using action::EncoderSections;
MULTIO_KEY_SET_DESCRIPTION(EncoderInfoDef,                                                //
                           "encoder-configuration",                                       //
                                                                                          //
                           KeyDef<EncoderInfoDef::Name, std::string>{"name"},             //
                           KeyDef<EncoderInfoDef::Tag, std::string>{"tag"},               //
                           nestedKeyDef<EncoderInfoDef::Sections, EncoderSectionsDef>(),  //
                           KeyDef<EncoderInfoDef::Sample, std::string>{"sample"}.tagOptional())


};  // namespace datamod


namespace action {

using EncoderInfoKeySet = datamod::KeySet<EncoderInfoDef>;
using EncoderInfoKeyValueSet = datamod::KeyValueSet<EncoderInfoKeySet>;
using EncoderInfo = EncoderInfoKeyValueSet;

//---------------------------------------------------------------------------------------------------------------------

}  // namespace action
}  // namespace multio

