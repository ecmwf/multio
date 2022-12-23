ecbuild_add_library(

	TARGET multio-action-aggregation

    SOURCES
    	Aggregation.cc
    	Aggregation.h


    PRIVATE_INCLUDES
      ${ECKIT_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
        multio
)
