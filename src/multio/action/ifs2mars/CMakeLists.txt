ecbuild_add_library(

    TARGET multio-action-ifs2mars

    TYPE SHARED # Due to reliance on factory self registration this library cannot be static

    SOURCES
        ifs2mars.cc
        ifs2mars.h

    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}

    PUBLIC_LIBS
        multio
)

