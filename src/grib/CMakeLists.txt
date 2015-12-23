### grib sources

list( APPEND grib_srcs
    GribHandle.h
    GribHandle.cc )

### grib library

ecbuild_add_library(

    TARGET grib

    INSTALL_HEADERS LISTED

    HEADER_DESTINATION
        ${INSTALL_INCLUDE_DIR}/grib

    SOURCES
        ${grib_srcs}

    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}
        ${GRIB_API_INCLUDE_DIRS}

    LIBS
        eckit grib_api )
