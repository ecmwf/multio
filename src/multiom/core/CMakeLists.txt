set(MULTIOM_CORE_DIR ${CMAKE_CURRENT_SOURCE_DIR})

#
# Enter in the module
message("${OFFSET} + core sources: ${MULTIOM_CORE_DIR}")

#
# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})


#
# Add Subdirectories#
add_subdirectory( common )
add_subdirectory( configuration )
add_subdirectory( hooks )


#
# Path to the configured file
set(MULTIO_OUTPUT_MANAGER_VERSION_FILE ${CMAKE_CURRENT_BINARY_DIR}/output_manager_version_mod.F90)

# Remove the file if it exists
if(EXISTS ${MULTIO_OUTPUT_MANAGER_VERSION_FILE})
    file(REMOVE ${MULTIO_OUTPUT_MANAGER_VERSION_FILE})
endif()

# Generate the file
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/output_manager_version_mod.F90.in
  ${MULTIO_OUTPUT_MANAGER_VERSION_FILE}
)


#
# Main sources
set( MULTIOM_CORE_MAIN_SOURCES
  ${CMAKE_CURRENT_BINARY_DIR}/output_manager_version_mod.F90
)


#
# Collect source files in module2
set( MULTIOM_CORE_SOURCES
  ${MULTIOM_CORE_MAIN_SOURCES}
  ${MULTIOM_COMMON_SOURCES}
  ${MULTIOM_CONFIGURATION_SOURCES}
  ${MULTIOM_HOOKS_SOURCES}
  CACHE INTERNAL "List of all sources in core directory"
)


#
# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_CORE_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()


#
# Create the library
ecbuild_add_library(
  TARGET
    multiom-core

  SOURCES
    ${MULTIOM_COMMON_SOURCES}
    ${MULTIOM_CONFIGURATION_SOURCES}
    ${MULTIOM_HOOKS_SOURCES}
    ${CMAKE_CURRENT_BINARY_DIR}/output_manager_version_mod.F90

  DEFINITIONS
    ${OUTPUT_MANAGER_BUILD_FLAVOUR}

  PRIVATE_INCLUDES
    $<BUILD_INTERFACE:${MULTIO_OUTPUT_MANAGER_INCLUDE_DIR}>

  PUBLIC_LIBS
    fckit
    eccodes
    eccodes_f90
)


#
# Module directory
set_target_properties( multiom-core PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/module/multiom )
target_include_directories( multiom-core PUBLIC $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/module/multiom> )