ecbuild_add_library(

	TARGET multio-action-aggregate

    SOURCES
    	Aggregate.cc
    	Aggregate.h


    PRIVATE_INCLUDES
      ${ECKIT_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
        multio
)
