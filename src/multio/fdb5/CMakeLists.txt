ecbuild_add_library(

    TARGET multio-fdb5

    SOURCES
        FDB5Sink.cc
        FDB5Sink.h

    CONDITION
        HAVE_FDB5

    PRIVATE_INCLUDES
      ${FDB5_INCLUDE_DIRS}
      ${ECKIT_INCLUDE_DIRS}

    PUBLIC_LIBS
        multio
        fdb5
)
