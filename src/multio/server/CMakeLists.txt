
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
        Aggregator.cc
        Aggregator.h
        Dispatcher.cc
        Dispatcher.h
        IoTransport.cc
        IoTransport.h
        Listener.cc
        Listener.h
        LocalIndices.cc
        LocalIndices.h
        Mappings.cc
        Mappings.h
        Message.cc
        Message.h
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

    PRIVATE_INCLUDES
        ${ECCODES_INCLUDE_DIRS}
        ${ECKIT_INCLUDE_DIRS}

    LIBS
        multio
        multio-fdb5
        eckit_mpi
        eckit_option
        eckit
        eccodes
        eccodes_f90 # why do we link to eccodes_f90 ??
)

ecbuild_add_executable( TARGET    multio-hammer
                        NOINSTALL
                        CONDITION HAVE_MULTIO_SERVER
                        SOURCES   tools/multio-hammer.cc
                        INCLUDES  ${ECCODES_INCLUDE_DIRS}
                                  ${METKIT_INCLUDE_DIRS}
                        LIBS      multio-server multio)

ecbuild_add_executable( TARGET    multio-grib
                        NOINSTALL
                        CONDITION HAVE_MULTIO_SERVER
                        SOURCES   tools/multio-grib.cc
                        INCLUDES  ${ECCODES_INCLUDE_DIRS}
                                  ${ECKIT_INCLUDE_DIRS}
                        LIBS      multio-server multio)
