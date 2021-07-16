ecbuild_add_library(

    TARGET multio-maestro

    SOURCES
        MaestroCdo.cc
        MaestroCdo.h
        MaestroSelector.cc
        MaestroSelector.h
        MaestroSubscription.cc
        MaestroSubscription.h
        MaestroEvent.cc
        MaestroEvent.h
        MaestroSource.cc
        MaestroSource.h
        MaestroSink.cc
        MaestroSink.h
        ThreadsafeMap.h

    CONDITION
        HAVE_MAESTRO

    PUBLIC_LIBS
        multio
        maestro
        eckit_mpi
        pgen
)
