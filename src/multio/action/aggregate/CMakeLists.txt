ecbuild_add_library(

    TARGET multio-action-aggregate

    TYPE SHARED # Due to reliance on factory self registration this library cannot be static

    SOURCES
    	Aggregate.cc
    	Aggregate.h
        AggregationCatalogue.cc
        AggregationCatalogue.cc

    PRIVATE_INCLUDES
      ${ECKIT_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
        multio
)
