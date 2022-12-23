ecbuild_add_library(

	TARGET multio-action-singleFieldSink

    SOURCES
    	SingleFieldSink.cc
    	SingleFieldSink.h


    PRIVATE_INCLUDES
      ${ECKIT_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
        multio
)
