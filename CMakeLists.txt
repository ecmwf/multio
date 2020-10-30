# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

########################################################################################################################

cmake_minimum_required( VERSION 3.12 FATAL_ERROR )

find_package( ecbuild 3.4 REQUIRED HINTS ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/../ecbuild)

project( multio LANGUAGES CXX )

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

### dependencies

ecbuild_find_package( NAME eccodes VERSION  2.18  REQUIRED )
ecbuild_find_package( NAME eckit   VERSION  1.13 REQUIRED )
ecbuild_find_package( NAME metkit  VERSION  1.4  REQUIRED )

if( NOT metkit_HAVE_GRIB )
  ecbuild_critical( "metkit must be built with GRIB support" )
endif()

### fdb5 plugin

ecbuild_add_option( FEATURE FDB5
                    DEFAULT ON
                    DESCRIPTION "Send data to FDB5"
                    REQUIRED_PACKAGES "NAME fdb5 VERSION 5.0" )

### legacy IFS I/O

ecbuild_add_option( FEATURE LEGACY_IFSIO
                    DEFAULT OFF
                    DESCRIPTION "Legacy (FDB4) IFS I/O interface" )

### multio-server

ecbuild_add_option( FEATURE MULTIO_SERVER
                    DEFAULT OFF
                    CONDITION eckit_HAVE_MPI
                    DESCRIPTION "Multio server" )

### Maestro plugin

ecbuild_add_option( FEATURE MAESTRO
                    DEFAULT OFF
                    DESCRIPTION "Pass data to Maestro middleware"
                    REQUIRED_PACKAGES "NAME Maestro" )

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
