ecbuild_add_library(

	TARGET multio-action-encode-grib2

    TYPE SHARED # Due to reliance on factory self registration this library cannot be static

    SOURCES
      EncodeGrib2.cc
      EncodeGrib2.h
      GridInfo.cc
      GridInfo.h
      Exception.cc
      Exception.h
      SampleManager.cc
      SampleManager.h

    PRIVATE_INCLUDES
      ${ECKIT_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
        multio
        atlas
)
