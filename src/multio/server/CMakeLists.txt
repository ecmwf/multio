
list (APPEND multio_server_deps
    multio
    eckit_mpi
    eckit_option
    eckit
    metkit
    eccodes
)

if( HAVE_FDB5 )
    list( APPEND multio_server_deps
        multio-fdb5
    )
endif()

ecbuild_add_library(

    TARGET multio-server

    CONDITION HAVE_MULTIO_SERVER

    SOURCES
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
        MultioNemo.cc
        MultioNemo.h
        PlainDataBlob.cc
        PlainDataBlob.h
        ThreadTransport.cc
        ThreadTransport.h
        MpiTransport.cc
        MpiTransport.h
        TcpTransport.cc
        TcpTransport.h
        Transport.cc
        Transport.h
        ScopedThread.h
        MultioServerTool.cc
        MultioServerTool.h
        multio_nemo.f90
        multio.f90

    LIBS
        ${multio_server_deps}
)

ecbuild_add_executable( TARGET    multio-hammer
                        CONDITION HAVE_MULTIO_SERVER
                        SOURCES   tools/multio-hammer.cc
                        LIBS      multio-server multio)

ecbuild_add_executable( TARGET    multio-replay
                        CONDITION HAVE_MULTIO_SERVER
                        SOURCES   tools/multio-replay.cc
                        LIBS      multio-server multio)

ecbuild_add_executable( TARGET    multio-probe
                        CONDITION HAVE_MULTIO_SERVER
                        SOURCES   tools/multio-probe.cc
                        LIBS      multio-server multio)

ecbuild_add_executable( TARGET    multio-encode-ocean
                        CONDITION HAVE_MULTIO_SERVER
                        SOURCES   tools/multio-encode-ocean.cc
                        LIBS      multio-server multio)

