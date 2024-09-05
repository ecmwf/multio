ecbuild_add_library(

    TARGET multio-action-ifs-mapping

    TYPE SHARED # Due to reliance on factory self registration this library cannot be static

    SOURCES
    	IfsMapping.cc
    	IfsMapping.h


    PRIVATE_INCLUDES
      ${ECKIT_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
        multio
)
