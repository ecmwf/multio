#pragma once

#include "multio/datamod/DataModelling.h"
#include "multio/datamod/GribKeys.h"
#include "multio/datamod/GribTypes.h"
#include "multio/datamod/MarsMiscGeo.h"

#include "multio/mars2grib/sections/SectionSetter.h"

namespace multio {

// Level config
namespace mars2grib::sections {

enum class LevelDef : std::uint64_t
{
    Type,
    FixedLevel,
};
}

namespace datamod {
using mars2grib::sections::LevelDef;
MULTIO_KEY_SET_DESCRIPTION(
    LevelDef,                                                                                                   //
    "level-configurator",                                                                                       //
                                                                                                                //
    KeyDef<LevelDef::Type, datamod::TypeOfLevel>{"type"},                                                       //
    KeyDef<LevelDef::FixedLevel, datamod::KeyDefValueType_t<MarsKeys::LEVELIST>>{"fixed-level"}.tagOptional())  //
};  // namespace datamod


namespace mars2grib::sections {

using LevelKeySet = datamod::KeySet<LevelDef>;
using LevelKeyValueSet = datamod::KeyValueSet<LevelKeySet>;


// Determines the level and whether a level has to be set for a typeOfLevel
std::optional<datamod::KeyDefValueType_t<datamod::MarsKeys::LEVELIST>> levelForTypeOfLevel(
    const LevelKeyValueSet&, const datamod::MarsKeyValueSet&, const datamod::MiscKeyValueSet&);

// Determines the level and whether a level has to be set for a typeOfLevel
datamod::HorizontalKeyValueSet horizontalForTypeOfLevel(const LevelKeyValueSet&, const datamod::MarsKeyValueSet&,
                                                        const datamod::MiscKeyValueSet&);

std::optional<datamod::VerticalKeyValueSet> verticalForTypeOfLevel(const LevelKeyValueSet&,
                                                                   const datamod::MarsKeyValueSet&,
                                                                   const datamod::MiscKeyValueSet&);

class LevelSetter : public DynSectionSetter {
public:
    DynSectionSetter::Config sectionInfo() const override;

    // Allocate is setting vertical
    void allocate(util::MioGribHandle&, const datamod::MarsKeyValueSet&, const datamod::MiscKeyValueSet&,
                  const datamod::Geometry&) const override;
    // Calls set level
    void preset(util::MioGribHandle&, const datamod::MarsKeyValueSet&, const datamod::MiscKeyValueSet&,
                const datamod::Geometry&) const override;

    // Calls set level
    void runtime(util::MioGribHandle&, const datamod::MarsKeyValueSet&, const datamod::MiscKeyValueSet&,
                 const datamod::Geometry&) const override;

    void setLevels(util::MioGribHandle&, const datamod::MarsKeyValueSet&, const datamod::MiscKeyValueSet&,
                   const datamod::Geometry&) const;

    void check(const util::MioGribHandle&, const datamod::MarsKeyValueSet&, const datamod::MiscKeyValueSet&,
               const datamod::Geometry&) const override;

    void collectKeyInfo(KeyInfoList& required, KeyInfoList& optional, const datamod::MarsKeyValueSet&) const override;


    LevelSetter(const LevelKeyValueSet& conf) : conf_{conf} {}
    virtual ~LevelSetter() = default;

private:
    LevelKeyValueSet conf_;
};


}  // namespace mars2grib::sections
}  // namespace multio
