ecbuild_add_library(

	TARGET multio-action-sink

    SOURCES
    	Sink.cc
    	Sink.h


    PRIVATE_INCLUDES
      ${ECKIT_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
        multio
)
