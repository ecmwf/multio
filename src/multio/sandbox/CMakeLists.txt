
ecbuild_add_library(

    TARGET multio-sandbox

    CONDITION HAVE_MULTIO_SERVER

    SOURCES
        Message.cc
        Message.h
        Transport.cc
        Transport.h
        ThreadTransport.cc
        ThreadTransport.h
        SimpleTransport.cc
        SimpleTransport.h
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
