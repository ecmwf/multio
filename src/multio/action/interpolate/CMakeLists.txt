ecbuild_add_library(

    TARGET multio-action-interpolate

    TYPE SHARED # Due to reliance on factory self registration this library cannot be static

    SOURCES
        Interpolate.cc
        Interpolate.h

    PRIVATE_INCLUDES
        ${MIR_INCLUDE_DIRS}
        ${ECKIT_INCLUDE_DIRS}

    CONDITION
        HAVE_MIR

    PUBLIC_LIBS
        multio
        mir
)

ecbuild_add_executable(

    TARGET
        cache-generator-fesom-2-mir

    SOURCES
        FesomCacheGenerator.cc
        ../../tools/MultioTool.cc

    CONDITION
        HAVE_MIR AND HAVE_ATLAS_IO

    LIBS
        multio
        atlas_io
        eckit
        mir
)
