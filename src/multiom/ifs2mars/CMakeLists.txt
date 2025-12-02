set(MULTIOM_HOOKS_DIR ${CMAKE_CURRENT_SOURCE_DIR})

#
# Enter in the module
message("${OFFSET} + hooks sources: ${MULTIOM_HOOKS_DIR}")


#
# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})


#
# Add Subdirectories
add_subdirectory( ifs-interface )
add_subdirectory( output-manager )
add_subdirectory( ifs2mars )

#
# Main sources
set( MULTIOM_HOOKS_MAIN_SOURCES
  ${MULTIOM_HOOKS_DIR}/multiom_api.F90
)


#
# Collect source files in module2
set( MULTIOM_HOOKS_SOURCES
  ${MULTIOM_HOOKS_MAIN_SOURCES}
  ${MULTIOM_OUTPUT_MANAGER_SOURCES}
  ${MULTIOM_IFS_INTERFACE_SOURCES}
  ${MULTIOM_IFS2MARS_SOURCES}
  CACHE INTERNAL "List of all sources in hooks directory"
)

#
# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_HOOKS_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()


#
# Create the library
ecbuild_add_library(
  TARGET
    multiom

  SOURCES
    ${MULTIOM_OUTPUT_MANAGER_SOURCES}
    ${MULTIOM_IFS_INTERFACE_SOURCES}
    ${MULTIOM_IFS2MARS_SOURCES}
    ./multiom_api.F90

  DEFINITIONS
    ${OUTPUT_MANAGER_BUILD_FLAVOUR}

  PRIVATE_INCLUDES
    $<BUILD_INTERFACE:${MULTIO_OUTPUT_MANAGER_INCLUDE_DIR}>

  PUBLIC_LIBS
    fckit
    eccodes
    eccodes_f90
    multiom-core
    multiom-encoders
    multio-fapi
)


#
# Module directory
set_target_properties( multiom PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/module/multiom )
target_include_directories( multiom PUBLIC $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/module/multiom> )