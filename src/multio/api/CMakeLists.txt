# DataSink plugins

if( CMAKE_SYSTEM_NAME MATCHES "Linux" AND CMAKE_CXX_COMPILER_ID MATCHES "GNU" )
    list( APPEND multio_api_linker_options
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
)

if( HAVE_MIR )
    list( APPEND multio_action_plugins
        multio-action-interpolate
    )
endif()

# Create the c api library
add_subdirectory(c)

# Create the fortran api library
if( HAVE_FORTRAN )
add_subdirectory(fortran)
endif()