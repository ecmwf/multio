ecbuild_add_library(

    TARGET multio-action-single-field-sink

    TYPE SHARED # Due to reliance on factory self registration this library cannot be static

    SOURCES
        SingleFieldSink.cc
        SingleFieldSink.h


    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
        multio
)
