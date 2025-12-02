set(MULTIOM_RAWAPI_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Enter in the module
message("${OFFSET} + raw-api sources: ${MULTIOM_RAWAPI_DIR}")

# Recursive print
set(OFFSET ${OFFSET}${DOFFSET})

#
# Add Subdirectories
add_subdirectory(common)
add_subdirectory(dictionaries)
add_subdirectory(encoders)


# Collect source files in module2
set( MULTIOM_RAWAPI_MAIN_SOURCES
)

# Collect source files in module2
set( MULTIOM_RAWAPI_SOURCES
  ${MULTIOM_RAWAPI_MAIN_SOURCES}
  ${MULTIOM_RAWAPI_COMMON_SOURCES}
  ${MULTIOM_RAWAPI_DICTIONARIES_SOURCES}
  ${MULTIOM_RAWAPI_ENCODERS_SOURCES}
  CACHE INTERNAL "List of all sources in rawapi directory"
)

# Optionally, message what has been appended for debugging
foreach(source_file IN LISTS MULTIOM_RAWAPI_MAIN_SOURCES)

    # Extract the filename (without the directory)
    get_filename_component(filename ${source_file} NAME)

    # Print the filename
    message("${OFFSET}    - ${filename}")
endforeach()