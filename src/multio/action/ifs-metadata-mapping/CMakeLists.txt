ecbuild_add_library(

    TARGET
        multio-action-ifs-metadata-mapping

    SOURCES
        IFS_MetadataMapping.cc
        IFS_MetadataMapping.h

    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}

    PUBLIC_LIBS
        multio
)
