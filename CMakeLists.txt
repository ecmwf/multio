cmake_minimum_required( VERSION 3.6 FATAL_ERROR )

project( multio C CXX )

set( CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/../ecbuild/cmake")

include( ecbuild_system NO_POLICY_SCOPE )

ecbuild_requires_macro_version( 3.0 )

ecbuild_declare_project()

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

### dependencies

ecbuild_use_package( PROJECT eckit VERSION  1.8 REQUIRED )
ecbuild_use_package( PROJECT metkit VERSION 1.4 REQUIRED )
if( NOT METKIT_HAVE_GRIB )
  ecbuild_critical( "metkit must be built with GRIB support" )
endif()

### fdb5 plugin

ecbuild_add_option( FEATURE FDB5
                    DEFAULT ON
                    DESCRIPTION "Send data to FDB5"
                    REQUIRED_PACKAGES "PROJECT fdb5 VERSION 5.0" )

### multio-server

ecbuild_add_option( FEATURE MULTIO_SERVER
                    DEFAULT OFF
                    DESCRIPTION "Multio server"
                    REQUIRED_PACKAGES "PROJECT atlas VERSION 0.15" )

### sandbox

ecbuild_add_option( FEATURE SANDBOX
                    DEFAULT OFF
                    DESCRIPTION "Sanbox for exploring io server design" )


### export package info

set( MULTIO_INCLUDE_DIRS   ${CMAKE_CURRENT_SOURCE_DIR}/src ${CMAKE_CURRENT_BINARY_DIR}/src )
set( MULTIO_LIBRARIES      multio )

get_directory_property( MULTIO_DEFINITIONS COMPILE_DEFINITIONS )

### sources

include_directories( ${MULTIO_INCLUDE_DIRS} )

add_subdirectory( src )
add_subdirectory( tests )

### finalize project

ecbuild_pkgconfig(NAME multio
                  DESCRIPTION "ECMWF Multiplexing IO library"
                  LIBRARIES multio )

ecbuild_install_project( NAME ${PROJECT_NAME} )

ecbuild_print_summary()
