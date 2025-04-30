set(MULTIOM_API_WRAPPER_ENCODERS_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + encoders wrappers sources: ${MULTIOM_COMMON_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})

# Collect source files in module2
set( MULTIOM_API_WRAPPER_ENCODERS_MAIN_SOURCES
  ${MULTIOM_API_WRAPPER_ENCODERS_DIR}/cached_mapped_encoder_wrapper_mod.F90
)

# Collect source files in module2
set( MULTIOM_API_WRAPPER_ENCODERS_SOURCES
  ${MULTIOM_API_WRAPPER_ENCODERS_MAIN_SOURCES}
  CACHE INTERNAL "List of all sources in API directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_API_WRAPPER_ENCODERS_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()