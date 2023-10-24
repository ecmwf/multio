ecbuild_add_library(

	TARGET multio-action-healpix-ring2nest

  SOURCES
  	HEALPix_ring2nest.cc
   	HEALPix_ring2nest.h


  PRIVATE_INCLUDES

  CONDITION

  PUBLIC_LIBS
    multio
    atlas_io
)



ecbuild_add_executable(
    TARGET    cache-generator
    SOURCES   GenerateCache.cc ../../tools/MultioTool.cc HEALPix.cc HEALPix.h
    CONDITION HAVE_ATLAS_IO
    LIBS      multio atlas_io eckit
)