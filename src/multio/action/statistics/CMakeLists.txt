ecbuild_add_library(

	TARGET multio-action-statistics

    SOURCES
    	Statistics.cc
    	Statistics.h
      Period.cc
      Period.h
      Operation.h
      TemporalStatistics.cc
      TemporalStatistics.h


    PRIVATE_INCLUDES
      ${ECKIT_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
        multio
)
