ecbuild_add_library(

    TARGET multio-maestro

    TYPE SHARED # Due to reliance on factory self registration this library cannot be static

    SOURCES
        CdoNamer.h
        MaestroMetadata.h
        MaestroCdo.cc
        MaestroCdo.h
        MaestroEvent.cc
        MaestroEvent.h
        MaestroSelector.cc
        MaestroSelector.h
        MaestroSink.cc
        MaestroSink.h
        MaestroSource.cc
        MaestroSource.h
        MaestroStatistics.cc
        MaestroStatistics.h
        MaestroSubscription.cc
        MaestroSubscription.h
        MaestroWorker.cc
        MaestroWorker.h
        ThreadsafeMap.h

    CONDITION
        HAVE_MAESTRO

    PUBLIC_LIBS
        multio
        maestro
        eckit_mpi # For distributed producer
        pgen
)
