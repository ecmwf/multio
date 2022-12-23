ecbuild_add_library(

	TARGET multio-action-metadataMapping

    SOURCES
    	MetadataMapping.cc
    	MetadataMapping.h


    PRIVATE_INCLUDES
      ${ECKIT_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
        multio
)
