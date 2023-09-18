
ecbuild_add_library(

    TARGET multio-action-select

    SOURCES
        Select.cc
        Select.h

    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
        multio
)
