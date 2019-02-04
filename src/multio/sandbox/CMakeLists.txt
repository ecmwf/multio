
ecbuild_add_library(

    TARGET multio-sandbox

    CONDITION HAVE_MULTIO_SERVER

    SOURCES
        Message.cc
        Message.h
        Transport.cc
        Transport.h
        DummyTransport.cc
        DummyTransport.h
        Listener.cc
        Listener.h
        Dispatcher.cc
        Dispatcher.h

    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}
        ${ATLAS_INCLUDE_DIRS}

    LIBS
        multio
        eckit_mpi
        atlas
)
