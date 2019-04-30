
ecbuild_add_library(

    TARGET multio-server

    CONDITION HAVE_MULTIO_SERVER

    SOURCES
        actions/Aggregation.cc
        actions/Aggregation.h
        actions/AppendToFile.cc
        actions/AppendToFile.h
        actions/Encode.cc
        actions/Encode.h
        actions/Print.cc
        actions/Print.h
        actions/Select.cc
        actions/Select.h
        actions/Sink.cc
        actions/Sink.h
        actions/Null.cc
        actions/Null.h
        Action.cc
        Action.h
        Aggregator.cc
        Aggregator.h
        Dispatcher.cc
        Dispatcher.h
        Listener.cc
        Listener.h
        LocalIndices.cc
        LocalIndices.h
        Mappings.cc
        Mappings.h
        Message.cc
        Message.h
        Peer.cc
        Peer.h
        Plan.cc
        Plan.h
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

    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}

    LIBS
        multio
        eckit_mpi
        eckit_option
        eckit
)

include_directories( ${ECKIT_INCLUDE_DIRS} )

ecbuild_add_executable( TARGET    multio_server_thread_transport
                        NOINSTALL
                        SOURCES   tools/multio_server_thread_transport.cc
                        CONDITION HAVE_MULTIO_SERVER
                        LIBS      multio-server multio)

ecbuild_add_executable( TARGET    multio_server_mpi_transport
                        NOINSTALL
                        SOURCES   tools/multio_server_mpi_transport.cc
                        CONDITION HAVE_MULTIO_SERVER
                        LIBS      multio-server multio)

ecbuild_add_executable( TARGET    multio_server_tcp_transport
                        NOINSTALL
                        SOURCES   tools/multio_server_tcp_transport.cc
                        CONDITION HAVE_MULTIO_SERVER
                        LIBS      multio-server multio)
