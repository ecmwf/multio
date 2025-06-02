set(MULTIOM_API_WRAPPER_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Add subdirectories
# add_subdirectory( encoders )
add_subdirectory( dictionaries )


# Enter in the module
message("${OFFSET} + wrappers sources: ${MULTIOM_COMMON_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})

# Collect source files in module2
# set( MULTIOM_API_WRAPPER_MAIN_SOURCES
# )

# Collect source files in module2
set( MULTIOM_API_WRAPPER_SOURCES
  ${MULTIOM_API_WRAPPER_MAIN_SOURCES}
  ${MULTIOM_API_WRAPPER_DICTIONARIES_SOURCES}
  ${MULTIOM_API_WRAPPER_ENCODERS_SOURCES}
  CACHE INTERNAL "List of all sources in API directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_API_WRAPPER_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()
