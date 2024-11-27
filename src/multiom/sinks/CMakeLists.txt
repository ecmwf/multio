set(MULTIOM_SINKS_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} +sinks sources: ${MULTIOM_SINKS_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})


set( MULTIOM_SINKS_MAIN_SOURCES
  ${MULTIOM_SINKS_DIR}/sink_base_mod.F90
  ${MULTIOM_SINKS_DIR}/sink_factory_mod.F90
)

# Collect source files in module2
set( MULTIOM_SINKS_SOURCES
  ${MULTIOM_SINKS_MAIN_SOURCES}
  CACHE INTERNAL "List of all sources in sinks directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_SINKS_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()