ecbuild_add_library(

    TARGET multio-sandbox

    CONDITION HAVE_MULTIO_SERVER AND HAVE_SANDBOX

    SOURCES
        Dispatcher.cc
        Dispatcher.h
        Listener.cc
        Listener.h
        Message.cc
        Message.h
        Peer.cc
        Peer.h
        ThreadTransport.cc
        ThreadTransport.h
        Transport.cc
        Transport.h

    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}
        ${ATLAS_INCLUDE_DIRS}

    LIBS
        multio
        eckit
)
