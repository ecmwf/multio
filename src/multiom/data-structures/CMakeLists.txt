set(MULTIOM_DATA_STRUCTURES_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + data-structures sources: ${MULTIOM_DATA_STRUCTURES_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})

#
# Add Subdirectories
add_subdirectory(mars)
add_subdirectory(parametrization)

# Collect source files in data-structures
set( MULTIOM_DATA_STRUCTURES_MAIN_SOURCES
  ${MULTIOM_DATA_STRUCTURES_DIR}/time_utils_mod.F90
  ${MULTIOM_DATA_STRUCTURES_DIR}/cache_utils_mod.F90
  ${MULTIOM_DATA_STRUCTURES_DIR}/multiom_debug_mod.F90
)


# Collect source files in data-structures
set( MULTIOM_DATA_STRUCTURES_SOURCES
  ${MULTIOM_DATA_STRUCTURES_MARS_SOURCES}
  ${MULTIOM_DATA_STRUCTURES_PARAMETRIZATION_SOURCES}
  ${MULTIOM_DATA_STRUCTURES_MAIN_SOURCES}
  CACHE INTERNAL "List of all sources in data-structures directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_DATA_STRUCTURES_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()
