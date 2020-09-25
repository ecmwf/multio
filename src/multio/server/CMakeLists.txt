if( HAVE_FDB5 )
    list( APPEND multio_server_plugins
        multio-fdb5
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
        ConfigurationPath.h
        Dispatcher.cc
        Dispatcher.h
        GribTemplate.h
        GribTemplate.cc
        IoTransport.cc
        IoTransport.h
        Listener.cc
        Listener.h
        MultioClient.cc
        MultioClient.h
        MultioServer.cc
        MultioServer.h
        NemoToGrib.cc
        NemoToGrib.h
        MultioNemo.cc
        MultioNemo.h
        ThreadTransport.cc
        ThreadTransport.h
        MpiTransport.cc
        MpiTransport.h
        TcpTransport.cc
        TcpTransport.h
        Transport.cc
        Transport.h
        ScopedThread.h
        multio_nemo.f90
        multio.f90

    PUBLIC_LIBS
        ${multio_server_plugins}
        multio
        eckit_mpi
        eckit_option
        eckit
        metkit
        eccodes
)
