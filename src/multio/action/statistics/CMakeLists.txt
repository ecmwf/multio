
list( APPEND _statistics_sources
    cfg/StatisticsConfiguration.cc
    cfg/StatisticsConfiguration.h
    cfg/StatisticsOptions.cc
    cfg/StatisticsOptions.h
    periodUpdaters/PeriodUpdater.h
    periodUpdaters/HourPeriodUpdater.h
    periodUpdaters/DayPeriodUpdater.h
    periodUpdaters/MonthPeriodUpdater.h
    StatisticsIO.cc
    StatisticsIO.h
    io/FstreamIO.cc
    io/FstreamIO.h
    TimeUtils.cc
    TimeUtils.h
    PeriodUpdaters.cc
    PeriodUpdaters.h
    OperationWindow.cc
    OperationWindow.h
    Operations.cc
    Operations.h
    operations/Operation.h
    operations/OperationWithData.h
    operations/OperationWithDeaccumulatedData.h
    operations/Accumulate.h
    operations/Average.h
    operations/FluxAverage.h
    operations/Instant.h
    operations/Minimum.h
    operations/Maximum.h
    operations/DeAccumulate.h
    operations/FixedWindowFluxAverage.h
    TemporalStatistics.cc
    TemporalStatistics.h
    Statistics.cc
    Statistics.h
)

list( APPEND _statistics_libs
    multio
)

if(HAVE_ATLAS_IO)

ecbuild_find_package( NAME atlas_io    VERSION  0.33   REQUIRED )

list( APPEND _statistics_sources
    io/AtlasIO.cc
    io/AtlasIO.h
)

list( APPEND _statistics_libs
    atlas_io
)

endif()

ecbuild_add_library(

    TARGET multio-action-statistics

    SOURCES
        ${_statistics_sources}


    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
    ${_statistics_libs}
)
