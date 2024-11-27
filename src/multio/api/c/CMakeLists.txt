
# Action plugins

if( CMAKE_SYSTEM_NAME MATCHES "Linux" AND CMAKE_CXX_COMPILER_ID MATCHES "GNU" )
    list( APPEND multio_action_plugins
        -Wl,--no-as-needed
    )
endif()

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
    multio-action-scale
)

if( HAVE_MIR )
    list( APPEND multio_action_plugins
        multio-action-interpolate
    )
endif()

# Source files
list( APPEND multio_capi_srcs
    multio_capi_types.h
    multio_capi_constants.h
    multio_capi_error_handling.h
    multio_capi_utils.h
    multio_capi_configuration.h
    multio_capi_base_handle.h
    multio_capi_handle.h
    multio_capi_metadata.h
    multio_capi_data.h
    multio_c.h
    multio_c_cpp_utils.h
    multio_capi.cc
)


# Create the library
if ( HAVE_MULTIO_DUMMY_API )
ecbuild_add_library(

    TARGET multio-api

    SOURCES
        ${multio_capi_srcs}

    PRIVATE_DEFINITIONS
        MULTIO_DUMMY_API

    PUBLIC_LIBS
        ${multio_action_plugins}
        multio
)

else()

ecbuild_add_library(

    TARGET multio-api

    SOURCES
        ${multio_capi_srcs}

    PUBLIC_LIBS
        ${multio_action_plugins}
        multio
)
endif()
