ecbuild_add_library(

    TARGET multio-action-mask

    TYPE SHARED # Due to reliance on factory self registration this library cannot be static

    SOURCES
        Mask.cc
        Mask.h


    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
        multio
)
