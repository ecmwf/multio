list( APPEND _statistics_sources
StatisticsConfiguration.cc
StatisticsConfiguration.h
StatisticsIO.cc
StatisticsIO.h
io/FstreamIO.cc
io/FstreamIO.h
TimeUtils.cc
TimeUtils.h
PeriodUpdater.cc
PeriodUpdater.h
MovingWindow.cc
MovingWindow.h
Operations.cc
Operations.h
operations/OperationBase.h
operations/OperationWithData.h
operations/Accumulate.h
operations/Average.h
operations/FluxAverage.h
operations/Instant.h
operations/Minimum.h
operations/Maximum.h
TemporalStatistics.cc
TemporalStatistics.h
Statistics.cc
Statistics.h )



ecbuild_add_library(

    TARGET multio-action-statistics

    SOURCES
        ${_statistics_sources}


    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}
        ${ATLAS_IO_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
        multio atlas_io
)
