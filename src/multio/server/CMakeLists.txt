
ecbuild_add_library(

    TARGET multio-server

    CONDITION HAVE_MULTIO_SERVER

    SOURCES
        Action.cc
        Action.h
        Aggregation.cc
        Aggregation.h
        Dispatcher.cc
        Dispatcher.h
        Distributor.cc
        Distributor.h
        Mappings.cc
        Mappings.h
        Message.cc
        Message.h
        Plan.cc
        Plan.h
        PlanAssembler.cc
        PlanAssembler.h
        SerialisationHelpers.cc
        SerialisationHelpers.h
        Transport.h
        Transport.cc
        MpiTransport.h
        MpiTransport.cc
        Select.cc
        Select.h
        Sink.cc
        Sink.h
        sandbox/Transport.h
        sandbox/Transport.cc
        sandbox/DummyTransport.h
        sandbox/DummyTransport.cc

    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}
        ${ATLAS_INCLUDE_DIRS}

    LIBS
        multio
        eckit_mpi
        atlas
)
