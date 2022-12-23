ecbuild_add_library(

	TARGET multio-action-statistics

    SOURCES
    	Statistics.cc
    	Statistics.h


    PRIVATE_INCLUDES
      ${ECKIT_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
        multio
)
