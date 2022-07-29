
if( HAVE_MULTIO_SERVER )
    ecbuild_enable_fortran( REQUIRED MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/module )
endif()

ecbuild_add_library(

    TARGET multio-api

    CONDITION HAVE_MULTIO_SERVER

    SOURCES
        multio_c.cc
        multio_c.h

    PUBLIC_LIBS
        multio-server
)
