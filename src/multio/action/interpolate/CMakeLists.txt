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
        HEALPix.cc
        HEALPix.h
        ../../tools/MultioTool.cc

    CONDITION
        HAVE_MIR AND HAVE_ECKIT_CODEC AND HAVE_MULTIO_BUILD_TOOLS

    LIBS
        multio
        eckit
        eckit_codec
        mir
)
