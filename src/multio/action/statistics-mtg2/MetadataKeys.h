#pragma once

#include "multio/datamod/core/EntryDef.h"
#include "multio/datamod/GribKeys.h"
#include "multio/datamod/MarsMiscGeo.h"
#include "multio/datamod/MarsKeys.h"

namespace multio::action::statistics_mtg2 {

//----------------------------- FlushKind Enum ------------------------------//

enum class FlushKind : std::size_t
{
    Default,
    FirstStep,
    StepAndRestart,
    LastStep,
    EndOfSimulation,
    CloseConnection
};

}  // namespace multio::action::statistics_mtg2

//-------------- FlushKind type parsing/dumping specializations --------------//

template <>
struct multio::datamod::DumpType<multio::action::statistics_mtg2::FlushKind> {
    static std::string dump(multio::action::statistics_mtg2::FlushKind v);
};

template <>
struct multio::datamod::ParseType<multio::action::statistics_mtg2::FlushKind> {
    static multio::action::statistics_mtg2::FlushKind parse(const std::string& s);
    static multio::action::statistics_mtg2::FlushKind parse(std::int64_t val);
};

//--------------------------------- Records ---------------------------------//

namespace multio::action::statistics_mtg2 {

namespace dm = multio::datamod;

//-------------------------- Flush-local EntryDefs --------------------------//

constexpr auto FLUSH_KIND =
    dm::EntryDef<FlushKind>{"flushKind"}
        .withDefault(FlushKind::Default)
        .withAccessor([](auto&& v) { return &v.flushKind; });

constexpr auto RESTART_DATE_TIME =
    dm::EntryDef<std::string>{"restartDateTime"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.restartDateTime; });

constexpr auto SERVER_RANK =
    dm::EntryDef<std::int64_t>{"serverRank"}
        .tagOptional()
        .withAccessor([](auto&& v) { return &v.serverRank; });

//----------------------- Field Metadata Keys Record ------------------------//

struct FieldMetadataKeys {
    dm::EntryType_t<decltype(dm::DATE)> date;
    dm::EntryType_t<decltype(dm::TIME)> time;
    dm::EntryType_t<decltype(dm::STEP)> step;
    dm::EntryType_t<decltype(dm::TIMESPAN)> timespan;
    dm::EntryType_t<decltype(dm::PARAM)> param;
    dm::EntryType_t<decltype(dm::STREAM)> stream;
    dm::EntryType_t<decltype(dm::LEVTYPE)> levtype;
    dm::EntryType_t<decltype(dm::LEVELIST)> levelist;
    dm::EntryType_t<decltype(dm::GRID)> grid;
    dm::EntryType_t<decltype(dm::TRUNCATION)> truncation;
    dm::EntryType_t<decltype(dm::TimeIncrementInSeconds)> timeIncrementInSeconds;
    dm::EntryType_t<decltype(dm::BitmapPresent)> bitmapPresent;
    dm::EntryType_t<decltype(dm::MissingValue)> missingValue;

    static constexpr std::string_view record_name_ = "statistics-mtg2-field";
    static constexpr auto record_entries_ = std::make_tuple(
        dm::DATE,
        dm::TIME,
        dm::STEP.tagRequired(),
        dm::TIMESPAN,
        dm::PARAM,
        dm::STREAM.tagOptional(),
        dm::LEVTYPE.tagRequired(),
        dm::LEVELIST,
        dm::GRID,
        dm::TRUNCATION,
        dm::TimeIncrementInSeconds,
        dm::BitmapPresent,
        dm::MissingValue
    );
};

//------------------------ Flush Metadata Keys Record -----------------------//

struct FlushMetadataKeys {
    dm::EntryType_t<decltype(dm::DATE)> date;
    dm::EntryType_t<decltype(dm::TIME)> time;
    dm::EntryType_t<decltype(dm::STEP)> step;
    dm::EntryType_t<decltype(FLUSH_KIND)> flushKind;
    dm::EntryType_t<decltype(RESTART_DATE_TIME)> restartDateTime;
    dm::EntryType_t<decltype(SERVER_RANK)> serverRank;

    static constexpr std::string_view record_name_ = "statistics-mtg2-flush";
    static constexpr auto record_entries_ = std::make_tuple(
        dm::DATE.tagOptional(),
        dm::TIME.tagOptional(),
        dm::STEP,
        FLUSH_KIND,
        RESTART_DATE_TIME,
        SERVER_RANK
    );
};

//---------------------------------------------------------------------------//

}  // namespace multio::action::statistics_mtg2
