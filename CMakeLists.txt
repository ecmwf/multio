cmake_minimum_required( VERSION 2.8.12 FATAL_ERROR )

project( multio C CXX )

set( CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/../ecbuild/cmake")

include( ecbuild_system NO_POLICY_SCOPE )

ecbuild_requires_macro_version( 2.0 )

### dependencies

# eckit multiplexer

ecbuild_use_package( PROJECT eckit VERSION  0.11 REQUIRED )

ecbuild_debug("   ECKIT_FEATURES : [${ECKIT_FEATURES}]" )

if( NOT ECKIT_HAVE_EXPERIMENTAL )
   ecbuild_fatal( "eckit hasn't EXPERIMENTAL support" )
endif()

# fdb

ecbuild_add_option( FEATURE FDB
                    DEFAULT ON
                    DESCRIPTION "Send data to FDB4"
                    REQUIRED_PACKAGES "PROJECT fdb   VERSION  4.7" )

### start project

ecbuild_declare_project()

### export package info

set( MULTIO_INCLUDE_DIRS   ${CMAKE_CURRENT_SOURCE_DIR}/src ${CMAKE_CURRENT_BINARY_DIR}/src )
set( MULTIO_LIBRARIES      multio )

get_directory_property( MULTIO_DEFINITIONS COMPILE_DEFINITIONS )

### sources

include_directories( ${MULTIO_INCLUDE_DIRS} )

add_subdirectory( src )

### finalize project

ecbuild_pkgconfig(NAME multio
                  DESCRIPTION "ECMWF IO library"
                  LIBRARIES multio )

ecbuild_install_project( NAME ${PROJECT_NAME} )

ecbuild_print_summary()
