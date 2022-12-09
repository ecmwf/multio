

ecbuild_add_library(

    TARGET multio-api

    CONDITION HAVE_MULTIO_SERVER

    SOURCES
        multio_c.cc
        multio_c.h

    PUBLIC_LIBS
        multio-server
)

# Cannot use CONDITION for (ecbuild_)configure_file
if( HAVE_MULTIO_SERVER AND HAVE_FORTRAN )
    configure_file( multio_config.f90.in multio_config.f90 )
endif()

ecbuild_add_library(

    TARGET multio-fapi

    CONDITION HAVE_MULTIO_SERVER AND HAVE_FORTRAN

    SOURCES
        multio_api.f90
        ${CMAKE_CURRENT_BINARY_DIR}/multio_config.f90

    PUBLIC_INCLUDES
        $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/module>

    PUBLIC_LIBS
        multio-api
)

