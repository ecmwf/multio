set(MULTIOM_METADATA_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + metadata sources: ${MULTIOM_METADATA_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})

# Main operations
set( MULTIOM_METADATA_MAIN_SOURCES
  ${MULTIOM_METADATA_DIR}/encoder_tracer_mod.F90
  ${MULTIOM_METADATA_DIR}/metadata_base_mod.F90
  ${MULTIOM_METADATA_DIR}/metadata_list_mod.F90
  ${MULTIOM_METADATA_DIR}/metadata_factory_mod.F90
  ${MULTIOM_METADATA_DIR}/grib_metadata_mod.F90
  # ${MULTIOM_METADATA_DIR}/multIO_metadata_mod.F90
  ${MULTIOM_METADATA_DIR}/log_metadata_mod.F90
  # ${MULTIOM_METADATA_DIR}/metadata_tracer_mod.F90
  ${MULTIOM_METADATA_DIR}/metadata_trace_node_mod.F90
)

# Collect source files in module2
set( MULTIOM_METADATA_SOURCES
  ${MULTIOM_METADATA_MAIN_SOURCES}
  CACHE INTERNAL "List of all sources in metadata directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_METADATA_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()