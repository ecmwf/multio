
if( CMAKE_SYSTEM_NAME MATCHES "Linux" AND CMAKE_CXX_COMPILER_ID MATCHES "GNU" )
    list( APPEND multio_server_plugins
        -Wl,--no-as-needed
    )
endif()

if( HAVE_FDB5 )
    list( APPEND multio_server_plugins
        multio-fdb5
    )
endif()

if( HAVE_MAESTRO )
    list( APPEND multio_server_plugins
        multio-maestro
    )
endif()

ecbuild_add_library(

    TARGET multio-server

    CONDITION HAVE_MULTIO_SERVER

    SOURCES
        Dispatcher.cc
        Dispatcher.h
        IoTransport.cc
        IoTransport.h
        Listener.cc
        Listener.h
        MultioClient.cc
        MultioClient.h
        MultioErrorHandling.cc
        MultioErrorHandling.h
        NemoToGrib.cc
        NemoToGrib.h
        MultioNemo.cc
        MultioNemo.h
        MultioServer.cc
        MultioServer.h

    PUBLIC_LIBS
        ${multio_server_plugins}
        multio
        eckit
        eckit_mpi
)

if( HAVE_MULTIO_SERVER AND HAVE_FORTRAN )
    ecbuild_enable_fortran( REQUIRED MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/module )
endif()

ecbuild_add_library(

    TARGET multio-server-fapi

    CONDITION HAVE_MULTIO_SERVER AND HAVE_FORTRAN

    SOURCES
        multio_nemo.f90
        multio.f90

    PUBLIC_INCLUDES
        $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/module>

    PUBLIC_LIBS
        multio-server
)
