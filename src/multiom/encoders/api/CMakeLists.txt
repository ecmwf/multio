set(MULTIOM_API_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + api sources: ${MULTIOM_API_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})

#
# Add Subdirectories
add_subdirectory(common)
add_subdirectory(dictionaries)


# Collect source files in module2
set( MULTIOM_API_MAIN_SOURCES
)

# Collect source files in module2
set( MULTIOM_API_SOURCES
  ${MULTIOM_API_MAIN_SOURCES}
  ${MULTIOM_API_COMMON_SOURCES}
  ${MULTIOM_API_DICTIONARIES_SOURCES}
  CACHE INTERNAL "List of all sources in api directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_API_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()