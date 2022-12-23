ecbuild_add_library(

	TARGET multio-action-encode

    SOURCES
    	Encode.cc
    	Encode.h


    PRIVATE_INCLUDES
      ${ECKIT_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
        multio
)
