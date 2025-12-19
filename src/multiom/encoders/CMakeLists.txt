set(MULTIOM_ENCODERS_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + encoders sources: ${MULTIOM_ENCODERS_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})


#
# Add Subdirectories
add_subdirectory( dictionaries )
add_subdirectory( api )


# Collect source files in module2
set( MULTIOM_ENCODERS_SOURCES
    ${MULTIOM_DATA_STRUCTURES_SOURCES}
    ${MULTIOM_ENCODERS_SOURCES}
    ${MULTIOM_API_SOURCES}
  CACHE INTERNAL "List of all sources in encoders directory"
)


#
# Create the library
ecbuild_add_library(
  TARGET
    multiom-encoders

  SOURCES
    ${MULTIOM_DATA_STRUCTURES_SOURCES}
    ${MULTIOM_API_SOURCES}

  DEFINITIONS
    ${OUTPUT_MANAGER_BUILD_FLAVOUR}

  PRIVATE_INCLUDES
    $<BUILD_INTERFACE:${MULTIO_OUTPUT_MANAGER_INCLUDE_DIR}>

  PUBLIC_LIBS
    fckit
    eccodes
    eccodes_f90
    multiom-core
)


#
# Module directory
set_target_properties( multiom-encoders PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/module/multiom )
target_include_directories( multiom-encoders PUBLIC $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/module/multiom> )
