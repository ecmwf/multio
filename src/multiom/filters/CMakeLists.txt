set(MULTIOM_FILTERS_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + filters sources: ${MULTIOM_FILTERS_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})

#
# Add Subdirectories
add_subdirectory( custom )
add_subdirectory( mars )

#
# Local files
set(MULTIOM_FILTERS_MAIN_SOURCES
  ${MULTIOM_FILTERS_DIR}/filter_base_mod.F90
  ${MULTIOM_FILTERS_DIR}/filter_composed_mod.F90
  ${MULTIOM_FILTERS_DIR}/filter_factory_mod.F90
  # ${MULTIOM_FILTERS_DIR}/filter_options_mod.F90
)

# Collect source files in module2
set( MULTIOM_FILTERS_SOURCES
  ${MULTIOM_FILTERS_MAIN_SOURCES}
  ${MULTIOM_FILTERS_CUSTOM_SOURCES}
  ${MULTIOM_FILTERS_MARS_SOURCES}
  CACHE INTERNAL "List of all sources in filters directory"
)



#
# + BUILD OUTPUT MANAGER LIBRARY
# ======================================================================
# ecbuild_add_library(
#   TARGET  multiom-filters
#   SOURCES ${MULTIOM_FILTERS_SOURCES}
#
#   DEFINITIONS ${OUTPUT_MANAGER_BUILD_FLAVOUR}
#
#   PRIVATE_INCLUDES
#     ${CMAKE_BINARY_DIR}/include
#     ${OUTPUT_MANAGER_INCLUDE_PATH}
#
#   PUBLIC_LIBS
#     fckit
#     eccodes
#     eccodes_f90
#     multio-fapi
# )

# Module directory
# set_target_properties( multiom-filters PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/module/multiom )
# target_include_directories( multiom-filters PUBLIC $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/module/multiom> )

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_FILTERS_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()