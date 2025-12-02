set(MULTIOM_DATA_STRUCTURES_GEOMETRY_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + parametrization sources: ${MULTIOM_DATA_STRUCTURES_GEOMETRY_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})


# Collect source files in data-structures
set( MULTIOM_DATA_STRUCTURES_GEOMETRY_MAIN_SOURCES
  ${MULTIOM_DATA_STRUCTURES_GEOMETRY_DIR}/representations_mod.F90
)

# Collect source files in data-structures
set( MULTIOM_DATA_STRUCTURES_GEOMETRY_SOURCES
  ${MULTIOM_DATA_STRUCTURES_GEOMETRY_MAIN_SOURCES}
  CACHE INTERNAL "List of all sources in data-structures/mars directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_DATA_STRUCTURES_GEOMETRY_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()