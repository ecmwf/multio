
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
        actions/SingleFieldSink.cc
        actions/SingleFieldSink.h
        actions/Null.cc
        actions/Null.h
        Action.cc
        Action.h
        Dispatcher.cc
        Dispatcher.h
        Domain.cc
        Domain.h
        GribTemplate.h
        GribTemplate.cc
        IoTransport.cc
        IoTransport.h
        Listener.cc
        Listener.h
        Mappings.cc
        Mappings.h
        Message.cc
        Message.h
        Metadata.h
        MultioClient.cc
        MultioClient.h
        MultioNemo.cc
        MultioNemo.h
        MessageContent.cc
        MessageHeader.cc
        Peer.cc
        Peer.h
        Plan.cc
        Plan.h
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

    LIBS
        multio
        multio-fdb5
        eckit_mpi
        eckit_option
        eckit
        metkit
        eccodes
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
                        SOURCES   tools/multio-server.cc
                        LIBS      multio-server multio)
