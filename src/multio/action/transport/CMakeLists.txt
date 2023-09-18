ecbuild_add_library(

    TARGET multio-action-transport

    TYPE SHARED # Due to reliance on factory self registration this library cannot be static

    SOURCES
        Transport.cc
        Transport.h

    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
        multio
)
