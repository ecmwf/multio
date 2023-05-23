ecbuild_add_library(

	TARGET multio-action-encode

    SOURCES
      Encode.cc
      Encode.h
      GribEncoder.cc
      GribEncoder.h
      GridDownloader.cc
      GridDownloader.h
      MioGribHandle.cc
      MioGribHandle.h

    PRIVATE_INCLUDES
      ${ECKIT_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
        multio
        atlas
)
