# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

########################################################################################################################

cmake_minimum_required( VERSION 3.12 FATAL_ERROR )

find_package( ecbuild 3.6 REQUIRED HINTS ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/../ecbuild)

project( multio LANGUAGES CXX )

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

### dependencies

ecbuild_find_package( NAME eccodes     VERSION  2.18   REQUIRED )
ecbuild_find_package( NAME eckit       VERSION  1.22   REQUIRED )
ecbuild_find_package( NAME metkit      VERSION  1.7    REQUIRED )
ecbuild_find_package( NAME atlas       VERSION  0.35   REQUIRED )

if( NOT metkit_HAVE_GRIB )
  ecbuild_critical( "metkit must be built with GRIB support" )
endif()

if( NOT eckit_HAVE_MPI )
  ecbuild_critical( "eckit must be built with MPI support" )
endif()

### fdb5 plugin

ecbuild_add_option( FEATURE FDB5
                    DEFAULT ON
                    DESCRIPTION "Send data to FDB5"
                    REQUIRED_PACKAGES "NAME fdb5 VERSION 5.10.1" )

### atlas-io plugin

ecbuild_add_option( FEATURE ATLAS_IO
                    DEFAULT ON
                    DESCRIPTION "Use atlasIO for the restart"
                    REQUIRED_PACKAGES "NAME atlas_io VERSION 0.33" )

### interpolate action

ecbuild_add_option( FEATURE MIR
                    DEFAULT ON
                    DESCRIPTION "Interpolate data using MIR"
                    REQUIRED_PACKAGES "NAME mir VERSION 1.16.0" )

ecbuild_add_option( FEATURE PGEN
                    DEFAULT OFF
                    DESCRIPTION "Use pgen for Maestro product-generation workflow"
                    REQUIRED_PACKAGES "NAME pgen VERSION 1.13.0" )

ecbuild_add_option( FEATURE FORTRAN
                    DEFAULT ON
                    DESCRIPTION "Build Fortran API and examples" )

### legacy IFS I/O

ecbuild_add_option( FEATURE LEGACY_IFSIO
                    DEFAULT OFF
                    DESCRIPTION "Legacy (FDB4) IFS I/O interface" )


if( HAVE_FORTRAN )
  ecbuild_enable_fortran( REQUIRED MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/module)
  ecbuild_find_package( NAME fckit   VERSION 0.10.0   REQUIRED )
  find_package( MPI COMPONENTS Fortran) # Add explicit mpi for errorcodes -- isn't there a better way?
  include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/fortran_compile_flags.cmake)
endif()


### Maestro plugin

ecbuild_add_option( FEATURE MAESTRO
                    DEFAULT OFF
                    DESCRIPTION "Pass data to Maestro middleware"
                    REQUIRED_PACKAGES "NAME Maestro" )

### sandbox

ecbuild_add_option( FEATURE SANDBOX
                    DEFAULT OFF
                    DESCRIPTION "Sanbox for exploring io server design" )

### HEALPix tests

ecbuild_add_option( FEATURE HEALPIX_EXAMPLES
                    DEFAULT OFF
                    DESCRIPTION "Enable creation of HEALPix examples" )


###  Definitions used by the output manager
ecbuild_add_option( FEATURE OUTPUT_MANAGER
                    DEFAULT OFF
                    DESCRIPTION "Build the output manager" )

ecbuild_add_option( FEATURE OUTPUT_MANAGER_ENCODER_REPORT
                    CONDITION HAVE_OUTPUT_MANAGER
                    DEFAULT OFF
                    DESCRIPTION "Enable generation of the encoding report" )

ecbuild_add_option( FEATURE OUTPUT_MANAGER_TOOL
                    CONDITION HAVE_OUTPUT_MANAGER
                    DEFAULT OFF
                    DESCRIPTION "Enable generation of standalone tool" )

ecbuild_add_option( FEATURE MULTIO_SERVER_MEMORY_PROFILE
                    DEFAULT OFF
                    DESCRIPTION "Enable multio server memory profiling" )

### export package info

set( MULTIO_CONFIG_DIR     share/multio/config )
set( MULTIO_INCLUDE_DIRS   ${CMAKE_CURRENT_SOURCE_DIR}/src ${CMAKE_CURRENT_BINARY_DIR}/src )
set( MULTIO_LIBRARIES      multio )

get_directory_property( MULTIO_DEFINITIONS COMPILE_DEFINITIONS )

### sources

include(cmake/compiler_warnings.cmake) # optionally handle compiler specific warnings


include_directories( ${MULTIO_INCLUDE_DIRS} )

add_subdirectory( src )
add_subdirectory( tests )

### finalize project

ecbuild_pkgconfig(NAME multio
                  DESCRIPTION "ECMWF Multiplexing IO library"
                  LIBRARIES multio )

ecbuild_install_project( NAME ${PROJECT_NAME} )

if( HAVE_FORTRAN AND ECBUILD_INSTALL_FORTRAN_MODULES )
    install( DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/${CMAKE_CFG_INTDIR}
             DESTINATION module/${PROJECT_NAME}
             COMPONENT modules )
endif()


ecbuild_print_summary()
