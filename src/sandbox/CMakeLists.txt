
ecbuild_add_library(

    TARGET multio-sandbox

    CONDITION HAVE_MULTIO_SERVER AND HAVE_SANDBOX

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

ecbuild_add_executable( TARGET   multio_sandbox_thread_transport
                        NOINSTALL
                        SOURCES  multio_sandbox_thread_transport.cc
                        CONDITION HAVE_MULTIO_SERVER AND HAVE_SANDBOX
                        LIBS     multio-sandbox multio)

ecbuild_add_executable( TARGET   multio_sandbox_mpi_transport
                        NOINSTALL
                        SOURCES  multio_sandbox_mpi_transport.cc
                        CONDITION HAVE_MULTIO_SERVER AND HAVE_SANDBOX
                        LIBS     multio-sandbox multio)

ecbuild_add_executable( TARGET   multio_sandbox_tcp_transport
                        NOINSTALL
                        SOURCES  multio_sandbox_tcp_transport.cc
                        CONDITION HAVE_MULTIO_SERVER AND HAVE_SANDBOX
                        LIBS     multio-sandbox multio)
