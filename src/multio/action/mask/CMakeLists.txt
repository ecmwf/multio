ecbuild_add_library(

	TARGET multio-action-mask

    SOURCES
    	Mask.cc
    	Mask.h


    PRIVATE_INCLUDES
      ${ECKIT_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
        multio
)
