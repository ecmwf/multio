ecbuild_add_library(

    TARGET multio-fdb5

    SOURCES
        Fdb5DirectSink.cc
        Fdb5DirectSink.h
        FDB5Sink.cc
        FDB5Sink.h

    CONDITION
        HAVE_FDB5

    PRIVATE_INCLUDES
        ${FDB5_INCLUDE_DIRS}
        ${ECKIT_INCLUDE_DIRS}

    LIBS
        multio
        fdb5
)
