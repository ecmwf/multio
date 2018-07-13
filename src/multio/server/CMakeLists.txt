
ecbuild_add_library(

    TARGET multio-server

    SOURCES
        Action.cc
        Action.h
        Aggregation.cc
        Aggregation.h
        Dispatcher.cc
        Dispatcher.h
        Distributor.cc
        Distributor.h
        Message.cc
        Message.h
        Plan.cc
        Plan.h
        PlanFactory.cc
        PlanFactory.h
        SerialisationHelpers.cc
        SerialisationHelpers.h
        Transport.h
        Transport.cc
        MpiTransport.h
        MpiTransport.cc
        Sink.cc
        Sink.h

    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}
        ${ATLAS_INCLUDE_DIRS}

    LIBS
        multio
        eckit_mpi
        atlas
)
