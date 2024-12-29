

if ( HAVE_OUTPUT_MANAGER )

# Update on new big features
set(OUTPUT_MANAGER_MAJOR 0)

# Interface changes
set(OUTPUT_MANAGER_MINOR 0)

# Implementation updates with constant interfaces
set(OUTPUT_MANAGER_PATCH 1)

# Define the include directory path
set(ECOM_INCLUDE_DIR ${CMAKE_CURRENT_BINARY_DIR}/include)

# Use Fortran to get the compiler and compiler version
execute_process(
    COMMAND ${CMAKE_Fortran_COMPILER} --version
    OUTPUT_VARIABLE FORTRAN_COMPILER_VERSION
    OUTPUT_STRIP_TRAILING_WHITESPACE
)
string(REGEX REPLACE "\n" ";" lines "${FORTRAN_COMPILER_VERSION}")
list(GET lines 0 FORTRAN_COMPILER_INFO)

# Linker flags (remove trailing spaces)
string(REGEX REPLACE "^[ \t]+" "" FORTRAN_BUILD_FLAGS         "${IFS_Fortran_FLAGS}")
string(REGEX REPLACE "^[ \t]+" "" FORTRAN_EXE_LINKER_FLAGS    "${ECBUILD_EXE_LINKER_FLAGS}")
string(REGEX REPLACE "^[ \t]+" "" FORTRAN_SHARED_LINKER_FLAGS "${ECBUILD_SHARED_LINKER_FLAGS}")

# Get the git sha of the current commit
execute_process(
    COMMAND git rev-parse HEAD
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/ifs-source
    OUTPUT_VARIABLE GIT_SHA
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

# Get the current date and time
string(TIMESTAMP BUILD_DATE "%Y-%m-%d %H:%M:%S")

# Get the build type (e.g., Debug, Release)
if(CMAKE_BUILD_TYPE)
    set(BUILD_TYPE ${CMAKE_BUILD_TYPE})
else()
    set(BUILD_TYPE "Unknown")
endif()

# Get the hostname
execute_process(
    COMMAND hostname
    OUTPUT_VARIABLE HOSTNAME
    OUTPUT_STRIP_TRAILING_WHITESPACE
)


# Path to the configured file
set(ECOM_VERSION_FILE ${CMAKE_CURRENT_BINARY_DIR}/output_manager_version_mod.F90)

# Remove the file if it exists
if(EXISTS ${ECOM_VERSION_FILE})
    file(REMOVE ${ECOM_VERSION_FILE})
endif()

# Generate the file
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/output_manager_version_mod.F90.in
  ${ECOM_VERSION_FILE}
)


# Clean the directory if it exists
if(EXISTS ${ECOM_INCLUDE_DIR})
    file(REMOVE_RECURSE ${ECOM_INCLUDE_DIR})
endif()

# Create the directory
file(MAKE_DIRECTORY ${ECOM_INCLUDE_DIR})


#
# + CONFIGURE HEADERS
# ======================================================================
configure_file(  ${CMAKE_CURRENT_SOURCE_DIR}/include/standard/output_manager_preprocessor_utils.h.in         ${ECOM_INCLUDE_DIR}/output_manager_preprocessor_utils.h  )
configure_file(  ${CMAKE_CURRENT_SOURCE_DIR}/include/standard/output_manager_preprocessor_trace_utils.h.in   ${ECOM_INCLUDE_DIR}/output_manager_preprocessor_trace_utils.h  )
configure_file(  ${CMAKE_CURRENT_SOURCE_DIR}/include/standard/output_manager_preprocessor_logging_utils.h.in ${ECOM_INCLUDE_DIR}/output_manager_preprocessor_logging_utils.h  )
configure_file(  ${CMAKE_CURRENT_SOURCE_DIR}/include/standard/output_manager_preprocessor_errhdl_utils.h.in  ${ECOM_INCLUDE_DIR}/output_manager_preprocessor_errhdl_utils.h  )


#
# Recursive add fo all the files in the grib directory
set( OFFSET "" )
set( DOFFSET "     ")

#
# + BUILD OUTPUT MANAGER CORE (this organisation comes from dep. problems in CMake)
# =================================================================================

#
# + ADD SUB-DIRECTORIES
# =====================
add_subdirectory( common )
add_subdirectory( configuration )
add_subdirectory( containers )
add_subdirectory( data-structures )
add_subdirectory( hooks )
add_subdirectory( filters )
add_subdirectory( encoders )
add_subdirectory( metadata )
add_subdirectory( operations )
add_subdirectory( mapping-rules )
add_subdirectory( encoding-rules )
add_subdirectory( ifs-interface )
add_subdirectory( output-manager )
add_subdirectory( sinks )
add_subdirectory( ifs-encoders )
add_subdirectory( ifs2mars )
add_subdirectory( tools )
add_subdirectory( api )

#
# + ADD LOCAL FILES
# =================
list( APPEND output_manager_src
  ${MULTIOM_API_SOURCES}
  ${MULTIOM_COMMON_SOURCES}
  ${MULTIOM_CONFIGURATION_SOURCES}
  ${MULTIOM_CONTAINERS_SOURCES}
  ${MULTIOM_DATA_STRUCTURES_SOURCES}
  ${MULTIOM_HOOKS_SOURCES}
  ${MULTIOM_FILTERS_SOURCES}
  ${MULTIOM_ENCODERS_SOURCES}
  ${MULTIOM_METADATA_SOURCES}
  ${MULTIOM_OPERATIONS_SOURCES}
  ${MULTIOM_MAPPING_RULES_SOURCES}
  ${MULTIOM_ENCODING_RULES_SOURCES}
  ${MULTIOM_ENCODING_RULES_SOURCES}
  ${MULTIOM_IFS_INTERFACE_SOURCES}
  ${MULTIOM_OUTPUT_MANAGER_SOURCES}
  ${MULTIOM_SINKS_SOURCES}
  ${MULTIOM_IFS_ENCODERS_SOURCES}
  ${MULTIOM_IFS2MARS_SOURCES}
  ${CMAKE_CURRENT_BINARY_DIR}/output_manager_version_mod.F90
  ./multiom_api.F90
)

 message("All files in output_manager_src:")
foreach(source_file IN LISTS output_manager_src)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()

if ( HAVE_OUTPUT_MANAGER_ENCODER_REPORT )
list( APPEND OUTPUT_MANAGER_BUILD_FLAVOUR "WITH_OUTPUT_MANAGER_ENCODING_REPORT" )
endif()

#
# + BUILD OUTPUT MANAGER LIBRARY
# ======================================================================
message(STATUS "CMAKE_CURRENT_SOURCE_DIR: ${CMAKE_CURRENT_SOURCE_DIR}")
message(STATUS "CMAKE_CURRENT_BINARY_DIR: ${CMAKE_CURRENT_BINARY_DIR}")

ecbuild_add_library(
  TARGET  multiom
  SOURCES ${output_manager_src}

  DEFINITIONS ${OUTPUT_MANAGER_BUILD_FLAVOUR}

  PRIVATE_INCLUDES
    $<BUILD_INTERFACE:${ECOM_INCLUDE_DIR}>

  PUBLIC_LIBS
    fckit
    eccodes
    eccodes_f90
    multio-fapi
)


# Module directory
set_target_properties( multiom PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/module/multiom )
target_include_directories( multiom PUBLIC $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/module/multiom> )


# add tests
set(HAVE_OUTPUT_MANAGER_TESTS 1)
add_subdirectory( test )

endif()
