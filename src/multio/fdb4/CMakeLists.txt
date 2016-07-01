ecbuild_add_library(

    TARGET multio-fdb4

    SOURCES
        FDB4Sink.h
        FDB4Sink.cc

    PRIVATE_INCLUDES
        ${MULTIO_INCLUDE_DIRS}
        ${ECKIT_INCLUDE_DIRS}

    CONDITION HAVE_FDB4

    LIBS
        multio ifsio )
