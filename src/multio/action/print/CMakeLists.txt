ecbuild_add_library(

	TARGET multio-action-print

    SOURCES
    	Print.cc
    	Print.h


    PRIVATE_INCLUDES
      ${ECKIT_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
        multio
)
