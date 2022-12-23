
if( CMAKE_SYSTEM_NAME MATCHES "Linux" AND CMAKE_CXX_COMPILER_ID MATCHES "GNU" )
    list( APPEND multio_server_plugins
        -Wl,--no-as-needed
    )
endif()

list( APPEND multio_server_plugins
    multio-action-aggregation
    multio-action-encode
    multio-action-mask
    multio-action-metadataMapping
    multio-action-null
    multio-action-print
    multio-action-select
    multio-action-singleFieldSink
    multio-action-sink
    multio-action-statistics
    multio-action-transport
)

if( HAVE_FDB5 )
    list( APPEND multio_server_plugins
        multio-fdb5
    )
endif()

if( HAVE_MIR )
    list( APPEND multio_server_plugins
        multio-action-interpolate
    )
endif()

if( HAVE_MAESTRO )
    list( APPEND multio_server_plugins
        multio-maestro
    )
endif()

ecbuild_add_library(

    TARGET multio-server

    CONDITION HAVE_MULTIO_SERVER

    SOURCES
        Dispatcher.cc
        Dispatcher.h
        Listener.cc
        Listener.h
        MultioClient.cc
        MultioClient.h
        MultioServer.cc
        MultioServer.h

    PUBLIC_LIBS

        ${multio_server_plugins}
        multio
        eckit
        eckit_mpi
)
