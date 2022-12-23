ecbuild_add_library(

	TARGET multio-action-null

    SOURCES
    	Null.cc
    	Null.h


    PRIVATE_INCLUDES
      ${ECKIT_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
        multio
)
