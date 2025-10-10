#pragma once

#include "multio/datamod/GribKeys.h"
#include "multio/datamod/MarsMiscGeo.h"

#include "multio/datamod/core/EntryDef.h"
#include "multio/mars2grib/sections/SectionSetter.h"

// Level config
namespace multio::mars2grib::sections {

namespace dm = multio::datamod;

using LEVELIST_t = dm::EntryValueType_t<decltype(dm::LEVELIST)>;

constexpr auto LevelType = dm::EntryDef<dm::TypeOfLevel>{"type"}.withAccessor([](auto&& v) { return &v.type; });
constexpr auto FixedLevel = dm::EntryDef<LEVELIST_t>{"fixed-level"}  //
                                .tagOptional()                       //
                                .withAccessor([](auto&& v) { return &v.fixedLevel; });

struct LevelConfigurator {
    dm::EntryType_t<decltype(LevelType)> type;
    dm::EntryType_t<decltype(FixedLevel)> fixedLevel;

    static constexpr std::string_view record_name_ = "level-configurator";
    static constexpr auto record_entries_ = std::make_tuple(LevelType, FixedLevel);
};


// Determines the level and whether a level has to be set for a typeOfLevel
std::optional<LEVELIST_t> levelForTypeOfLevel(const LevelConfigurator&, const dm::FullMarsRecord&,
                                              const dm::MiscRecord&);

// Determines the level and whether a level has to be set for a typeOfLevel
dm::HorizontalGribKeys horizontalForTypeOfLevel(const LevelConfigurator&, const dm::FullMarsRecord&,
                                                const dm::MiscRecord&);

std::optional<dm::VerticalGribKeys> verticalForTypeOfLevel(const LevelConfigurator&, const dm::FullMarsRecord&,
                                                           const dm::MiscRecord&);

class LevelSetter : public DynSectionSetter {
public:
    DynSectionSetter::Config sectionInfo() const override;

    // Allocate is setting vertical
    void allocate(util::MioGribHandle&, const dm::FullMarsRecord&, const dm::MiscRecord&,
                  const dm::Geometry&) const override;
    // Calls set level
    void preset(util::MioGribHandle&, const dm::FullMarsRecord&, const dm::MiscRecord&,
                const dm::Geometry&) const override;

    // Calls set level
    void runtime(util::MioGribHandle&, const dm::FullMarsRecord&, const dm::MiscRecord&,
                 const dm::Geometry&) const override;

    void setLevels(util::MioGribHandle&, const dm::FullMarsRecord&, const dm::MiscRecord&, const dm::Geometry&) const;

    void check(const util::MioGribHandle&, const dm::FullMarsRecord&, const dm::MiscRecord&,
               const dm::Geometry&) const override;

    // void collectKeyInfo(KeyInfoList& required, KeyInfoList& optional, const dm::FullMarsRecord&) const override;


    LevelSetter(const LevelConfigurator& conf) : conf_{conf} {}
    virtual ~LevelSetter() = default;

private:
    LevelConfigurator conf_;
};


}  // namespace multio::mars2grib::sections


namespace multio::util {
template <>
struct Print<multio::mars2grib::sections::LevelConfigurator> : multio::datamod::PrintRecord {};

}  // namespace multio::util
