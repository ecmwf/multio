ecbuild_add_library(

    TARGET multio-maestro

    SOURCES
        MaestroSink.cc
        MaestroSink.h
        MaestroSelector.cc
        MaestroSelector.h
        MaestroSubscription.cc
        MaestroSubscription.h

    CONDITION
        HAVE_MAESTRO

    PUBLIC_LIBS
        multio
        maestro
        eckit_mpi
)
