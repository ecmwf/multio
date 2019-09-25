ecbuild_add_library(

    TARGET multio-fdb5

    SOURCES
        FDB5Sink.cc
        FDB5Sink.h

    CONDITION
        HAVE_FDB5

    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}

    LIBS
        multio
        fdb5
)
