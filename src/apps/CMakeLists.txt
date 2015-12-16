ecbuild_add_executable(
    TARGET mx
    SOURCES mx.cc
    INCLUDES ${ECKIT_INCLUDE_DIRS}
    LIBS multio
)

ecbuild_add_executable(

    TARGET multiplexer
    NOINSTALL

    CONDITION
        HAVE_GRIB

    SOURCES
        multiplexer.cc
        DummySource.cc
        DummySource.h

    INCLUDES
        ${ECKIT_INCLUDE_DIRS}
        ${GRIB_API_INCLUDE_DIRS}

    LIBS
        multio
        ${GRIB_API_LIBRARIES}
)

ecbuild_add_executable(
    TARGET readjournal
    SOURCES readjournal.cc
    INCLUDES ${ECKIT_INCLUDE_DIRS}
    LIBS multio
)
