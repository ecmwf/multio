
ecbuild_add_library(

    TARGET
        multio-action-interpolate-fesom

    SOURCES
        InterpolateFesom.cc
        InterpolateFesom.h
        InterpolateFesom_debug.h
        FesomInterpolationWeights.h
        FesomInterpolationWeights.cc

    PRIVATE_INCLUDES
        ${MIR_INCLUDE_DIRS}
        ${ECKIT_INCLUDE_DIRS}

    CONDITION
        HAVE_ECKIT_CODEC

    PUBLIC_LIBS
        multio
        eckit
        eckit_codec
)



ecbuild_add_executable(

    TARGET
        fesom-cache-generator

    SOURCES
        FesomCacheGenerator.cc
        ../../tools/MultioTool.cc
        FesomInterpolationWeights.h
        FesomInterpolationWeights.cc
        InterpolateFesom.h

    CONDITION
        HAVE_ECKIT_CODEC AND HAVE_MULTIO_BUILD_TOOLS

    LIBS
        multio
        eckit
        eckit_codec
)

ecbuild_add_executable(

    TARGET
        fesom-cache-validator

    SOURCES
        FesomCacheValidator.cc
        ../../tools/MultioTool.cc
        FesomInterpolationWeights.h
        FesomInterpolationWeights.cc
        InterpolateFesom.h

    CONDITION
        HAVE_ECKIT_CODEC AND HAVE_MULTIO_BUILD_TOOLS

    LIBS
        multio
        eckit
        eckit_codec
)

ecbuild_add_executable(

    TARGET
        fesom-spmvm-validator

    SOURCES
        FesomSpmvmValidator.cc
        ../../tools/MultioTool.cc
        FesomInterpolationWeights.h
        FesomInterpolationWeights.cc
        InterpolateFesom.h

    CONDITION
        HAVE_ECKIT_CODEC AND HAVE_MULTIO_BUILD_TOOLS

    LIBS
        multio
        eckit
        eckit_codec
)
