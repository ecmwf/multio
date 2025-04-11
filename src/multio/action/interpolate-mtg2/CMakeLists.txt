ecbuild_add_library(

    TARGET multio-action-interpolate-mtg2

    TYPE SHARED # Due to reliance on factory self registration this library cannot be static

    SOURCES
        Interpolate.cc
        Interpolate.h

    PRIVATE_INCLUDES
        ${MIR_INCLUDE_DIRS}
        ${ECKIT_INCLUDE_DIRS}

    CONDITION
        HAVE_MIR

    PUBLIC_LIBS
        multio
        mir
)
