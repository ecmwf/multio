ecbuild_add_library(

    TARGET multio-action-print

    TYPE SHARED # Due to reliance on factory self registration this library cannot be static

    SOURCES
        Print.cc
        Print.h

    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
        multio
)
