ecbuild_add_library(

    TARGET multio-action-transport

    SOURCES
        Transport.cc
        Transport.h

    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
        multio
)
