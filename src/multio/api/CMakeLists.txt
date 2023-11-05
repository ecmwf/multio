
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

# Action plugins

list( APPEND multio_action_plugins
    multio-action-aggregate
    multio-action-encode
    multio-action-mask
    multio-action-metadata-mapping
    multio-action-null
    multio-action-print
    multio-action-select
    multio-action-single-field-sink
    multio-action-sink
    multio-action-statistics
    multio-action-transport
    multio-action-renumber-healpix
    multio-action-interpolate-fesom
)

if( HAVE_MIR )
    list( APPEND multio_action_plugins
        multio-action-interpolate
    )
endif()


ecbuild_add_library(

    TARGET multio-api

    SOURCES
        multio_c.cc
        multio_c.h

    PUBLIC_LIBS
        ${multio_sink_plugins}
        ${multio_action_plugins}
        multio
)

# Cannot use CONDITION for (ecbuild_)configure_file
if( HAVE_FORTRAN )
    configure_file( multio_config.f90.in multio_config.f90 )
endif()

ecbuild_add_library(

    TARGET multio-fapi

    CONDITION HAVE_FORTRAN

    SOURCES
        multio_api.f90
        ${CMAKE_CURRENT_BINARY_DIR}/multio_config.f90

    PUBLIC_INCLUDES
        $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/module>

    PUBLIC_LIBS
        multio-api
)


ecbuild_add_library(

    TARGET multio-dummy-fapi

    CONDITION HAVE_FORTRAN

    SOURCES
        multio_dummy_api.f90

    PUBLIC_INCLUDES
        $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/module>

    PUBLIC_LIBS
)
