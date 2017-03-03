cmake_minimum_required( VERSION 2.8.11 FATAL_ERROR )

project( multio C CXX )

set( CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/../ecbuild/cmake")

include( ecbuild_system NO_POLICY_SCOPE )

ecbuild_requires_macro_version( 2.0 )

### dependencies

# eckit multiplexer

ecbuild_use_package( PROJECT eckit VERSION  0.10 REQUIRED )

### metkit must be built with GRIB support for GribDataBlobs
ecbuild_use_package( PROJECT metkit VERSION 0.3 REQUIRED )
if( NOT METKIT_HAVE_GRIB )
  ecbuild_critical( "metkit must be built with GRIB support" )
endif()

### fdb4 plugin

ecbuild_add_option( FEATURE FDB4
                    DEFAULT ON
                    CONDITION FDB_HAVE_IFSIO
                    DESCRIPTION "Send data to FDB4"
                    REQUIRED_PACKAGES "PROJECT fdb VERSION 4.8" )

### fdb5 plugin

ecbuild_add_option( FEATURE FDB5
                    DEFAULT ON
                    DESCRIPTION "Send data to FDB5"
                    REQUIRED_PACKAGES "PROJECT mars_server VERSION 7.5" )

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
                  DESCRIPTION "ECMWF Multiplexing IO library"
                  LIBRARIES multio )

ecbuild_install_project( NAME ${PROJECT_NAME} )

ecbuild_print_summary()
