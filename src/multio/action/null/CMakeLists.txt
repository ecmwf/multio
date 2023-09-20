ecbuild_add_library(

    TARGET multio-action-null

    TYPE SHARED # Due to reliance on factory self registration this library cannot be static

    SOURCES
        Null.cc
        Null.h


    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
        multio
)
