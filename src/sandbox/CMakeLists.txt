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

    LIBS
        multio
        eckit
)

include_directories( ${ECKIT_INCLUDE_DIRS} )

ecbuild_add_test( TARGET   test_multio_sandbox_thread_transport
                  SOURCES  test_multio_sandbox_thread_transport.cc
                  CONDITION HAVE_MULTIO_SERVER
                  LIBS     multio multio-sandbox eckit_option)
