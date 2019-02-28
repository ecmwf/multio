ecbuild_add_library(

    TARGET multio-fdb5

    SOURCES
        FDB5Sink.h
        FDB5Sink.cc

    CONDITION
        HAVE_FDB5

    PRIVATE_INCLUDES
        ${FDB5_INCLUDE_DIRS}
        ${ECKIT_INCLUDE_DIRS}

    LIBS
        multio
        fdb5
)
