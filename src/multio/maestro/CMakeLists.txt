
list( APPEND _multio_maestro_srcs
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
    MaestroStatistics.cc
    MaestroStatistics.h
    MaestroSubscription.cc
    MaestroSubscription.h
    ThreadsafeMap.h
)

if( HAVE_PGEN )
    list( APPEND _multio_maestro_srcs
        MaestroSource.cc
        MaestroSource.h
        MaestroWorker.cc
        MaestroWorker.h
    )
endif()

list( APPEND _link_libraries
    multio
    maestro
    eckit_mpi # For distributed producer
)

if ( HAVE_PGEN )
    list( APPEND _link_libraries
       pgen
    )
endif()

ecbuild_add_library(

    TARGET multio-maestro

    SOURCES
        ${_multio_maestro_srcs}

    CONDITION
        HAVE_MAESTRO

    PUBLIC_LIBS
        ${_link_libraries}
)
