# DataSink plugins

if( CMAKE_SYSTEM_NAME MATCHES "Linux" AND CMAKE_CXX_COMPILER_ID MATCHES "GNU" )
    list( APPEND multio_sink_plugins
        -Wl,--no-as-needed
    )
endif()

if( HAVE_FDB5 )
    list( APPEND multio_sink_plugins
        multio-fdb5
    )
endif()

if( HAVE_MAESTRO )
    list( APPEND multio_sink_plugins
        multio-maestro
    )
endif()

ecbuild_add_library(

    TARGET multio-action-sink

    TYPE SHARED # Due to reliance on factory self registration this library cannot be static

    SOURCES
        Sink.cc
        Sink.h

    PRIVATE_INCLUDES
        ${ECKIT_INCLUDE_DIRS}

    CONDITION

    PUBLIC_LIBS
        ${multio_sink_plugins}
        multio
)