
list( APPEND multio_fapi_srcs
    multio_fapi_constants.F90
    multio_fapi_error_handling.F90
    multio_fapi_configuration.F90
    multio_fapi_base_handle.F90
    multio_fapi_handle.F90
    multio_fapi_data.F90
    multio_fapi_metadata.F90
    multio_fapi_utils.F90
    multio_fapi.F90
)

if ( HAVE_MULTIO_DUMMY_API )
ecbuild_add_library(

    TARGET multio-fapi

    CONDITION HAVE_FORTRAN

    SOURCES
        ${multio_fapi_srcs}
        ${CMAKE_CURRENT_BINARY_DIR}/multio_config.f90

    PRIVATE_DEFINITIONS
        MULTIO_DUMMY_API

    PUBLIC_INCLUDES
        $<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>
        $<INSTALL_INTERFACE:module/${PROJECT_NAME}>

    PUBLIC_LIBS
        multio-api
)

else ()

ecbuild_add_library(

    TARGET multio-fapi

    CONDITION HAVE_FORTRAN

    SOURCES
        ${multio_fapi_srcs}

    PUBLIC_INCLUDES
        $<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>
        $<INSTALL_INTERFACE:module/${PROJECT_NAME}>

    PUBLIC_LIBS
        multio-api
)
endif()
