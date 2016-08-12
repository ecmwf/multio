ecbuild_add_library(

    TARGET multio-fdb4

    SOURCES
        FDB4Sink.h
        FDB4Sink.cc

    CONDITION
        HAVE_FDB4

    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}

    LIBS
        multio
        ifsio
)
