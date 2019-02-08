
ecbuild_add_library(

    TARGET multio-sandbox

    CONDITION HAVE_MULTIO_SERVER AND HAVE_SANDBOX

    SOURCES
        actions/AppendToFile.cc
        actions/AppendToFile.h
        actions/Print.cc
        actions/Print.h
        actions/Null.cc
        actions/Null.h
        Action.cc
        Action.h
        Dispatcher.cc
        Dispatcher.h
        Listener.cc
        Listener.h
        Message.cc
        Message.h
        Peer.cc
        Peer.h
        Plan.cc
        Plan.h
        ThreadTransport.cc
        ThreadTransport.h
        Transport.cc
        Transport.h
        ScopedThread.h

    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}

    LIBS
        multio
        eckit
)

include_directories( ${ECKIT_INCLUDE_DIRS} )

ecbuild_add_executable( TARGET   multio_sandbox_thread_transport
                NOINSTALL
                SOURCES  multio_sandbox_thread_transport.cc
                CONDITION HAVE_MULTIO_SERVER AND HAVE_SANDBOX
                LIBS     multio-sandbox multio eckit_option)

ecbuild_add_executable( TARGET   multio_sandbox_tcp_transport
                NOINSTALL
                SOURCES  multio_sandbox_tcp_transport.cc
                CONDITION HAVE_MULTIO_SERVER AND HAVE_SANDBOX
                LIBS     multio-sandbox multio eckit_option)
