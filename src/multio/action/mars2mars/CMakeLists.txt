ecbuild_add_library(
    TARGET multio-action-mars2mars

    TYPE SHARED # Due to reliance on factory self registration this library cannot be static

    SOURCES
        Mars2Mars.cc
        Mars2Mars.h

    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}

    CONDITION
        metkit_HAVE_MARS2MARS

    PUBLIC_LIBS
        multio
        metkit
)
