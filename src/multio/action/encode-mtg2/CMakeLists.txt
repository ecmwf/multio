ecbuild_add_library(
    TARGET multio-action-encode-mtg2

    TYPE SHARED # Due to reliance on factory self registration this library cannot be static

    SOURCES
        EncodeMtg2.cc
        EncodeMtg2.h

    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
        multio
        multiom-encoders
        eccodes
)
