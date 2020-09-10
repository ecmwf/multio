ecbuild_add_library(

    TARGET multio-grib

    SOURCES
        GribPacking.cc
        GribPacking.h

    CONDITION
        HAVE_FDB5

    PRIVATE_INCLUDES
      ${ECKIT_INCLUDE_DIRS}

    LIBS
        multio
)
