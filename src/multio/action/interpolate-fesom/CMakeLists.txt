ecbuild_add_library(
    TARGET           multio-action-interpolate-fesom
    SOURCES          InterpolateFESOM.cc InterpolateFESOM.h InterpolateFESOM_debug.h FesomInterpolationWeights.h FesomInterpolationWeights.cc
    PRIVATE_INCLUDES ${MIR_INCLUDE_DIRS} ${ECKIT_INCLUDE_DIRS}
    CONDITION        HAVE_ATLAS_IO
    PUBLIC_LIBS      multio atlas_io
)

ecbuild_add_executable(
    TARGET    fesom-cache-generator
    SOURCES   FesomCacheGenerator.cc ../../tools/MultioTool.cc FesomInterpolationWeights.h FesomInterpolationWeights.cc InterpolateFESOM.h
    CONDITION HAVE_ATLAS_IO
    LIBS      multio atlas_io eckit
)
