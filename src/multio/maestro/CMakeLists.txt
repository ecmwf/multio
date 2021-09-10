ecbuild_add_library(

    TARGET multio-maestro

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
        eckit_mpi
        pgen
)
