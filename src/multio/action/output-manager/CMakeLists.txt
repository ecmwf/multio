ecbuild_add_library(

    TARGET
        multio-action-output-manager

    SOURCES
        OutputManager.cc
        OutputManager.h

    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}

    PUBLIC_LIBS
        multio
)
