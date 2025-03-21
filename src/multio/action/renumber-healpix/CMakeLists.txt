
ecbuild_add_library(

    TARGET multio-action-renumber-healpix

    SOURCES
        HEALPix_ring2nest.cc
        HEALPix_ring2nest.h


    PRIVATE_INCLUDES

    PUBLIC_LIBS
        multio
        eckit_codec
)

# TODO: move this to the tools folder
ecbuild_add_executable(
    TARGET    multio-generate-healpix-cache
    SOURCES   GenerateCache.cc ${PROJECT_SOURCE_DIR}/src/multio/tools/MultioTool.cc HEALPix.cc HEALPix.h
    CONDITION HAVE_ECKIT_CODEC AND HAVE_MULTIO_BUILD_TOOLS
    LIBS      multio eckit eckit_codec
)
