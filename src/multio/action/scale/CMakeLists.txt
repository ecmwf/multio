ecbuild_add_library(

    TARGET multio-action-scale

    TYPE SHARED # Due to reliance on factory self registration this library cannot be static

    SOURCES
        Scale.cc
        Scale.h
        Mapping.cc
        Mapping.h
        Scaling.cc
        Scaling.h
        MetadataUtils.cc
        MetadataUtils.h

    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
        multio
)
