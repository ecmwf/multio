ecbuild_add_library(

    TARGET multiplexer

    SOURCES
        DataSink.cc
        DataSink.h
        DummySink.cc
        DummySink.h
        MultiplexerSink.cc
        MultiplexerSink.h

    INCLUDES
        ${ECKIT_INCLUDE_DIRS}

    LIBS
        eckit
        ${GRIB_API_LIBRARIES}
)


ecbuild_add_executable(

    TARGET multiplexer
    NOINSTALL

    CONDITION HAVE_GRIB

    SOURCES
        multiplexer.cc
        DummySource.cc
        DummySource.h

    INCLUDES
        ${ECKIT_INCLUDE_DIRS}
        ${GRIB_API_INCLUDE_DIRS}

    LIBS
        multiplexer ${GRIB_API_LIBRARIES}
)
